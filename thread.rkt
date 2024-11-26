#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define thread-mailbox-evt : (-> (Evtof Any))
  (lambda []
    (wrap-evt (thread-receive-evt)
              (λ _ (thread-receive)))))

(define thread-statistics : (->* () (Thread) (Values Boolean Boolean Boolean Natural))
  (let ([stat : (Vectorof (U Boolean Integer)) (vector #false #false #false 0)])
    (lambda [[thd (current-thread)]]
      (vector-set-performance-stats! stat thd)
      (values (and (vector-ref stat 0) #true)
              (and (vector-ref stat 1) #true)
              (and (vector-ref stat 2) #true)
              (let ([bs : (U Boolean Integer) (vector-ref stat 3)])
                (if (exact-nonnegative-integer? bs) bs 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define thread-safe-kill : (->* ((U Thread (Listof Thread))) (Nonnegative-Real) Void)
  (lambda [thds [timeout-s 2.0]]
    (define threads : (Listof Thread)
      (for/list ([thd (if (list? thds) (in-list thds) (in-value thds))])
        (break-thread thd)
        thd))

    (unless (andmap thread-dead? threads)
      (define breaker : Thread
        (thread (λ [] (let wait : Void ([thds : (Listof Thread) threads])
                        (define who (apply sync/enable-break thds))

                        (thread-wait who)
                        
                        (when (> (length thds) 1)
                          (wait (remove who thds)))))))

      (with-handlers ([exn:break? void])
        (sync/timeout/enable-break timeout-s breaker))
      
      (for ([thd (in-list (cons breaker threads))])
        (unless (thread-dead? thd)
          (kill-thread thd))))))

(define thread-safe-shutdown : (->* (Custodian Custodian) (Nonnegative-Real) Void)
  (lambda [self root [timeout-s 2.0]]
    (define thds : (Listof Thread)
      (let fold-thread : (Listof Thread) ([threads : (Listof Thread) null]
                                          [entities : (Listof Any) (custodian-managed-list self root)])
        (cond [(null? entities) threads]
              [else (let ([entity (car entities)])
                      (cond [(thread? entity) (fold-thread (cons entity threads) (cdr entities))]
                            [(custodian? entity) (fold-thread (fold-thread threads (custodian-managed-list entity root)) (cdr entities))]
                            [else (fold-thread threads (cdr entities))]))])))

    (thread-safe-kill thds timeout-s)))
