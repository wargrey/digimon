#lang racket/base

(provide (all-defined-out))

(require racket/class)
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define make-cheat-opaque?
  (lambda [? [fname #false]]
    (procedure-rename (cond [(procedure? ?) (λ [v] (and (? v) #true))]
                            [else (λ [v] (and (procedure? v) (procedure-arity-includes? v ? #true)))])
                      (or fname
                          (let ([maybe-name (object-name ?)])
                            (cond [(symbol? maybe-name) (string->symbol (format "cheat-~a" maybe-name))]
                                  [else 'cheat-opaque?]))))))

(define-syntax (define-cheat-opaque stx)
  (syntax-case stx []
    [(_ id #:=> FT arg) #'(define id (make-cheat-opaque? arg 'id))]
    [(_ id #:is-a? % arg%) #'(define id (make-cheat-opaque? (λ [v] (is-a? v arg%)) 'id))]
    [(_ id #:sub? % arg%) #'(define id (make-cheat-opaque? (λ [v] (subclass? v arg%)) 'id))]))

(define-syntax (define/make-is-a? stx)
  (syntax-case stx [:]
    [(_ % : Type% class-definition)
     (with-syntax ([%? (format-id #'% "~a?" (syntax-e #'%))])
       #'(begin (define % class-definition)
                (define-cheat-opaque %? #:is-a? Type% %)))]))
