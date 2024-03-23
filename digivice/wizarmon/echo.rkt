#lang typed/racket/base

(provide (all-defined-out))

(require "../../dtrace.rkt")
(require "../../continuation.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-wizarmon-log-trace : (-> Boolean (-> Void))
  (lambda [verbose?]
    (make-dtrace-loop #:default-receiver (wizarmon-event-echo verbose?)
                      (cond [(or verbose?) 'trace]
                            [else 'info]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-event-echo : (-> Boolean Dtrace-Receiver)
  (lambda [verbose?]
    (λ [level message urgent topic]
      (dtrace-event-echo level message urgent topic)

      (when (and (exn:fail? urgent) verbose?)
        (let ([/dev/stderr (open-output-string)])
          (display-continuation-stacks urgent /dev/stderr)
          (let ([errmsg (get-output-string /dev/stderr)])
            (unless (string=? errmsg "")
              (dtrace-event-echo 'trace (get-output-string /dev/stderr) #false topic))))))))
