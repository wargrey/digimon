#lang typed/racket/base

(provide (except-out (all-defined-out) $?-logging-routine-values))

(require "../expectation.rkt")
(require "../prompt.rkt")

(require "type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Log-Match-Datum
  (U (-> Any Boolean) Spec-Match-Datum
     (Vectorof (U (-> Any Boolean) True Spec-Match-Datum))
     (Listof (U (-> Any Boolean) Spec-Match-Datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $?-logging-routine-values : (Parameterof (Pairof Any (Listof Any))) (make-parameter (list (void))))

(define-spec-expectation (log-message [logsrc : Log-Receiver] [messages : Spec-Log-Match-Datum] [routine : (-> AnyValues)])
  (define-values (/dev/syncin /dev/syncout) (make-pipe))
  
  (define (message-okay? [given : (Immutable-Vector Symbol String Any (Option Symbol))] [expected : (U (-> Any Boolean) Spec-Match-Datum True)]) : Boolean
    (cond [(procedure? expected) (expected (vector-ref given 2))]
          [(boolean? expected) #true]
          [else (regexp-match? expected (vector-ref given 1))]))
  
  (define ghostcat
    (thread
     (λ []
       (define sgol : (Listof (Immutable-Vector Symbol String Any (Option Symbol)))
         (let collect ([sgol0 : (Listof (Immutable-Vector Symbol String Any (Option Symbol))) null])
           (define which (sync logsrc /dev/syncin))
           (cond [(input-port? which)
                  (read-byte /dev/syncin)
                  (let final-prove ([sgol : (Listof (Immutable-Vector Symbol String Any (Option Symbol))) sgol0])
                    (define msg (sync/timeout 0 logsrc))
                    (cond [(vector? msg) (final-prove (cons msg sgol))]
                          [else sgol]))]
                 [else (collect (cons which sgol0))])))
       
       (define okay? : Boolean
         (cond [(list? messages)
                (for/and ([log (in-list sgol)])
                  (for/or : Boolean ([msg (in-list messages)])
                    (message-okay? log msg)))]
               [(vector? messages)
                (and (eq? (length sgol) (vector-length messages))
                     (for/and ([log (in-list (reverse sgol))]
                               [msg (in-vector messages)])
                       (message-okay? log msg)))]
               [else (for/or ([log (in-list sgol)])
                       (message-okay? log messages))]))

       (write-byte (if okay? 1 0) /dev/syncout))))
  
  (call-with-values routine
    (λ results
      (cond [(pair? results) ($?-logging-routine-values results)]
            [else #| deadcode |# (spec-misbehave)])))
  
  (write-byte 0 /dev/syncout)
  (thread-wait ghostcat)
  (or (eq? (read-byte /dev/syncin) 1)
      (spec-misbehave)))

(define-spec-expectation (log-message* [logsrc : Log-Receiver] [messages : Spec-Log-Match-Datum] [routine : (-> AnyValues)]
                                       [check-routine : (U (-> (Pairof Any (Listof Any)) AnyValues) (Boxof (Pairof Any (Listof Any))))])
  (expect-log-message logsrc messages routine)
  
  (if (box? check-routine)
      (set-box! check-routine ($?-logging-routine-values))
      (check-routine ($?-logging-routine-values))))
