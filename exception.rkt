#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-simple-exception stx)
  (syntax-case stx [: lambda λ]
    [(_ exn:ssh:sub parent (fields ...) #:->* (Extra-Type ...) (Optional-Type ...) (lambda [args ... #:+ fmt argl] body ...))
     (with-syntax* ([make-exn (format-id #'exn:ssh:sub "make-~a" (syntax-e #'exn:ssh:sub))]
                    [make+exn (format-id #'exn:ssh:sub "make+~a" (syntax-e #'exn:ssh:sub))]
                    [throw-exn (format-id #'exn:ssh:sub "throw-~a" (syntax-e #'exn:ssh:sub))]
                    [throw+exn (format-id #'exn:ssh:sub "throw+~a" (syntax-e #'exn:ssh:sub))])
       #'(begin (struct exn:ssh:sub parent (fields ...))

                (define make-exn : (->* (Extra-Type ... String) (Optional-Type ...) #:rest Any exn:ssh:sub)
                  (lambda [args ... fmt . argl]
                    body ...))

                (define make+exn : (->* (Extra-Type ... String) (Optional-Type ...) #:rest Any exn:ssh:sub)
                  (lambda [args ... fmt . argl]
                    (let ([errobj (apply make-exn args ... fmt argl)])
                      (ssh-log-error errobj)
                      errobj)))

                (define throw-exn : (->* (Extra-Type ... String) (Optional-Type ...) #:rest Any Nothing)
                  (lambda [args ... fmt . argl]
                    (raise (apply make-exn args ... fmt argl))))

                (define throw+exn : (->* (Extra-Type ... String) (Optional-Type ...) #:rest Any Nothing)
                  (lambda [args ... fmt . argl]
                    (raise (apply make+exn args ... fmt argl))))))]
    [(_ exn:ssh:sub parent (fields ...) #:->* (Extra-Type ...) (Optional-Type ...) (λ [args ... #:+ fmt argl] body ...))
     #'(define-ssh-error exn:ssh:sub parent (fields ...) #:->* (Extra-Type ...) (Optional-Type ...) (lambda [args ... #:+ fmt argl] body ...))]))

(define-syntax (throw stx)
  (syntax-parse stx
    [(_ st:id rest ...)
     #'(throw [st] rest ...)]
    [(_ [st:id argl ...] frmt:str v ...)
     #'(throw [st argl ...] (#%function) frmt v ...)]
    [(_ [st:id argl ...] src frmt:str v ...)
     #'(raise (st (format (string-append "~s: " frmt) src v ...) (current-continuation-marks) argl ...))]))
