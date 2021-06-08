#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Listof+ Type) (Pairof Type (Listof Type)))
(define-type (Identity Type) Type)

(require/typed racket/class
               [#:struct (exn:fail:object exn:fail) ()
                #:extra-constructor-name make-exn:fail:object])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     (syntax/loc stx
       (begin (provide (all-from-out spec)) ...
              (require spec) ...))]))

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     (syntax/loc stx (require/typed/provide/batch modpath [id ...] type-definition))]
    [(_ modpath [id ...] type-definition)
     (syntax/loc stx (require/typed/provide modpath [id type-definition] ...))]))

(define-syntax (match/handlers stx)
  (syntax-case stx [:]
    [(_ s-exp : type? match-clause ...)
     (syntax/loc stx
       (match (with-handlers ([exn? (λ [[e : exn]] e)]) s-exp)
         match-clause ...
         [(? type? no-error-value) no-error-value]))]
    [(_ s-exp match-clause ...)
     (syntax/loc stx
       (match (with-handlers ([exn? (λ [[e : exn]] e)]) s-exp)
         match-clause ...
         [escaped-value escaped-value]))]))
