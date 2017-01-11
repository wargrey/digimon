#lang typed/racket/base

(provide (all-defined-out) make-cheat-opaque?)

(require typed/racket/unsafe)

(require racket/class)
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(unsafe-require/typed
 "digitama/cheat.rkt"
 [make-cheat-opaque? (All (FT) (->* ((U (-> Any Boolean) Byte)) ((Option Symbol)) (-> Any Boolean : #:+ FT)))])

;;; NOTE: Don't forget to keep the untyped macros consistent when modifying.
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
