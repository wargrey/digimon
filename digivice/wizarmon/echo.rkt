#lang typed/racket/base

(provide (all-defined-out))

(require digimon/dtrace)
(require digimon/continuation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-wizarmon-log-trace : (-> Boolean Boolean (-> Void))
  (lambda [debug? verbose?]
    (make-dtrace-loop #:default-receiver (wizarmon-event-echo debug? verbose?)
                      (cond [(or debug?) 'trace]
                            [(or verbose?) 'note]
                            [else 'info]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-event-echo : (-> Boolean Boolean Dtrace-Receiver)
  (lambda [debug? verbose?]
    (λ [level message urgent topic]
      (dtrace-event-echo level message urgent topic)

      (when (and (exn:fail? urgent) (or debug? verbose?))
        (let ([/dev/stderr (open-output-string)])
          (display-continuation-stacks urgent /dev/stderr)
          (let ([errmsg (get-output-string /dev/stderr)])
            (unless (string=? errmsg "")
              (dtrace-event-echo 'trace (get-output-string /dev/stderr) #false topic))))))))
