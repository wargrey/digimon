#lang typed/racket/base

(provide (all-defined-out))

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
