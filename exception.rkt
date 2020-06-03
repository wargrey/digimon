#lang typed/racket/base

(provide (all-defined-out))

(require "dtrace.rkt")
(require "format.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-exception stx)
  (syntax-parse stx #:literals [let :]
    [(_ eid (~optional maybe-parent) ([field : FieldType] ...) (make-message [arg : Type] ...) (~optional (~seq #:log maybe-log-exn:id)))
     (with-syntax* ([make-exn (format-id #'eid "make-~a" (syntax-e #'eid))]
                    [make+exn (format-id #'eid "make+~a" (syntax-e #'eid))]
                    [throw-exn (format-id #'eid "throw-~a" (syntax-e #'eid))]
                    [throw+exn (format-id #'eid "throw+~a" (syntax-e #'eid))]
                    [parent (or (attribute maybe-parent) #'exn:fail)]
                    [log-exn (or (attribute maybe-log-exn) #'dtrace-exception)])
       #'(begin (struct eid parent ([field : FieldType] ...) #:transparent)

                (define make-exn : (-> Any Type ... FieldType ... String Any * eid)
                  (lambda [src arg ... field ... fmt . argl]
                    (eid (make-message src arg ... field ... (~string fmt argl))
                         (current-continuation-marks)
                         field ...)))

                (define make+exn : (->* (Any Type ... FieldType ... String)
                                        (#:logger Logger #:level Log-Level #:topic (Option Symbol))
                                        #:rest Any eid)
                  (lambda [src arg ... field ...
                               #:logger [logger (current-logger)] #:level [level 'error] #:topic [topic dtrace-blank-topic]
                               fmt . argl]
                    (let ([errobj (make-exn src arg ... field ... (~string fmt argl))])
                      (log-exn errobj #:logger logger #:level level #:topic topic)
                      errobj)))

                (define throw-exn : (-> Any Type ... FieldType ... String Any * Nothing)
                  (lambda [src arg ... field ... fmt . argl]
                    (raise (make-exn src arg ... field ... (~string fmt argl)))))

                (define throw+exn : (->* (Any Type ... FieldType ... String)
                                         (#:logger Logger #:level Log-Level #:topic (Option Symbol))
                                         #:rest Any Nothing)
                  (lambda [src arg ... field ...
                               #:logger [logger (current-logger)] #:level [level 'error] #:topic [topic dtrace-blank-topic]
                               fmt . argl]
                    (raise (make+exn src arg ... field ... #:logger logger #:level level #:topic topic (~string fmt argl)))))))]))

(define-syntax (throw stx)
  (syntax-parse stx
    [(_ st:id rest ...)
     #'(throw [st] rest ...)]
    [(_ [st:id argl ...] frmt:str v ...)
     #'(throw [st argl ...] (#%function) frmt v ...)]
    [(_ [st:id argl ...] src frmt:str v ...)
     #'(raise (st (format (string-append "~s: " frmt) src v ...) (current-continuation-marks) argl ...))]))
