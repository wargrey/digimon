#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)

(require (for-syntax racket/base))
(require (for-syntax racket/symbol))
(require (for-syntax racket/syntax))

(require (for-syntax syntax/parse))

(define-type (Listof+ Type) (Pairof Type (Listof Type)))
(define-type (Identity Type) Type)

(require/typed racket/class
               [#:struct (exn:fail:object exn:fail) ()
                #:extra-constructor-name make-exn:fail:object])

(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     (syntax/loc stx
       (begin (provide (all-from-out spec)) ...
              (require spec) ...))]))

(define-syntax (require/provide/syntax stx)
  (syntax-case stx []
    [(_ spec ...)
     (syntax/loc stx
       (begin (provide (for-syntax (all-from-out spec))) ...
              (require (for-syntax spec)) ...))]))

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     (syntax/loc stx (require/typed/provide/batch modpath [id ...] type-definition))]
    [(_ modpath [id ...] type-definition)
     (syntax/loc stx (require/typed/provide modpath [id type-definition] ...))]))

(define-syntax (define-struct stx)
  (syntax-parse stx #:literals [:]
    [(_ id #:as ID (~optional (~seq #:with make-id))
        (~optional (~seq #:undefined Uv uv? uv) #:defaults ([Uv #'Void] [uv? #'void?] [uv #'(void)]))
        ([property : DataType info ...] ...) options ...)
     (with-syntax* ([make-instance (or (attribute make-id) (format-id #'id "make-~a" (syntax-e #'id)))]
                    [([property-filter ArgType defval ...] ...)
                     (for/list ([field-info (in-list (syntax->list #'([property DataType info ...] ...)))])
                       (syntax-parse field-info
                         [(p T #:= dv #:~> Super fltr) #'[(let ([v : (U T Uv) fltr]) (if (uv? v) dv v)) (U Super Uv) uv]]
                         [(p T #:= dv #:~> fltr) #'[(let ([v : (U T Uv) fltr]) (if (uv? v) dv v)) Any uv]]
                         [(p T #:~> Super fltr) #'[fltr Super]]
                         [(p T #:~> fltr) #'[fltr Any]]
                         [(p T #:= dv) #'[(if (uv? p) dv p) (U T Uv) uv]]
                         [(p T rest ...) #'[p T]]))]
                    [(args ...)
                     (for/fold ([args null])
                               ([argument (in-list (syntax->list #'([property : ArgType defval ...] ...)))])
                       (cons (datum->syntax argument (string->keyword (symbol->immutable-string (car (syntax->datum argument)))))
                             (cons argument args)))])
       (syntax/loc stx
         (begin (struct id ([property : DataType] ...) options ...)
                (define (make-instance args ...) : ID (id property-filter ...))
                (define-type ID id))))]))

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
