#lang typed/racket

(provide (all-defined-out))

(require typed/racket/unsafe)

(require (for-syntax racket/syntax))

(define-syntax (unsafe-require/typed/provide stx)
  (syntax-case stx []
    [(_ modpath [id Type] ...)
     #'(begin (provide id ...)
              (unsafe-require/typed modpath [id Type] ...))]))

(module ugly racket/base
  (provide (all-defined-out))

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
      [(_ id #:=> FT arg)
       #'(define id : (-> Any Boolean : #:+ FT)
           ((inst make-cheat-opaque? FT) arg 'id))]
      [(_ id #:is-a? % arg%)
       #'(define id : (-> Any Boolean : #:+ (Instance %))
           ((inst make-cheat-opaque? (Instance %)) (λ [v] (is-a? v arg%)) 'id))]
      [(_ id #:sub? % arg%)
       #'(define id : (-> Any Boolean : #:+ %)
           ((inst make-cheat-opaque? %) (λ [v] (subclass? v arg%)) 'id))]))
  
  (define-syntax (define/make-is-a? stx)
    (syntax-case stx [:]
      [(_ % : Type% class-definition)
       (with-syntax ([%? (format-id #'% "~a?" (syntax-e #'%))])
         #'(begin (define % : Type% class-definition)
                  (define-cheat-opaque %? #:is-a? Type% %)))])))

(unsafe-require/typed/provide
 (submod "." ugly)
 [make-cheat-opaque? (All (FT) (->* ((U (-> Any Boolean) Byte)) ((Option Symbol)) (-> Any Boolean : #:+ FT)))])


(define-syntax (define-cheat-opaque stx)
  (syntax-case stx []
    [(_ id #:=> FT arg)
     #'(define id : (-> Any Boolean : #:+ FT)
         ((inst make-cheat-opaque? FT) arg 'id))]
    [(_ id #:is-a? % arg%)
     #'(define id : (-> Any Boolean : #:+ (Instance %))
         ((inst make-cheat-opaque? (Instance %)) (λ [v] (is-a? v arg%)) 'id))]
    [(_ id #:sub? % arg%)
     #'(define id : (-> Any Boolean : #:+ %)
         ((inst make-cheat-opaque? %) (λ [v] (subclass? v arg%)) 'id))]))

(define-syntax (define/make-is-a? stx)
  (syntax-case stx [:]
    [(_ % : Type% class-definition)
     (with-syntax ([%? (format-id #'% "~a?" (syntax-e #'%))])
       #'(begin (define % : Type% class-definition)
                (define-cheat-opaque %? #:is-a? Type% %)))]))
