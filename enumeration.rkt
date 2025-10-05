#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax syntax/parse))

(require "digitama/ioexn.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-unnamed-enumeration stx)
  (syntax-case stx [:]
    [(_ id : BaseType #:with id-identity [enum ...])
     (syntax/loc stx
       (begin (define-unnamed-enumeration id : (U enum ...) [enum ...])
              
              (define id-identity : (All (a) (case-> [BaseType -> (U False enum ...)]
                                                     [BaseType (U enum ... (-> Symbol String BaseType a)) -> (U a enum ...)]))
                (let ([expected (exn-constraint->string (list 'enum ...))])
                  (case-lambda
                    [(e throw) (or (id-identity e) (if (procedure? throw) (throw 'id-identity expected e) throw))]
                    [(e) (cond [(eq? e enum) enum] ... [else #false])])))))]

    [(_ [id ids] : BaseType [enum ...])
     (with-syntax ([_ (let ([enums (syntax->list #'[enum ...])])
                        (for/list ([<enum> (in-value (check-duplicates enums eq? #:key syntax-e))])
                          (when (syntax? <enum>)
                            (raise-syntax-error 'define-unnamed-enumeration "duplicate value" <enum> #false
                                                (filter (λ [<e>] (and (eq? (syntax-e <e>) (syntax-e <enum>))
                                                                      (not (eq? <e> <enum>))))
                                                        enums)))))])
       (syntax/loc stx
         (begin (define ids : (Pairof BaseType (Listof BaseType)) (list 'enum ...)))))]

    [(_ id : BaseType [enum ...])
     (with-syntax ([ids (format-id #'id "~as" (syntax-e #'id))])
       (syntax/loc stx (define-unnamed-enumeration [id ids] : BaseType [enum ...])))]))

(define-syntax (define-enumeration stx)
  (syntax-case stx [:]
    [(_ id : TypeU #:with kw->enum [enum value] ...)
     (syntax/loc stx
       (begin (define-enumeration id : TypeU [enum ...])
              
              (define kw->enum : (All (a) (case-> [TypeU -> (U value ...)]
                                                  [Symbol -> (U False value ...)]
                                                  [Symbol (U TypeU (-> Symbol String Symbol a)) -> (U a value ...)]))
                (let ([expected (exn-constraint->string (list 'enum ...))])
                  (case-lambda
                    [(kw throw) (or (kw->enum kw) (if (symbol? throw) (kw->enum throw) (throw 'kw->enum expected kw)))]
                    [(kw) (cond [(eq? kw 'enum) value] ... [else #false])])))))]

    [(_ [id ids] : TypeU [enum ...])
     (with-syntax ([id? (format-id #'id "~a?" (syntax-e #'id))]
                   [_ (let ([enums (syntax->list #'[enum ...])])
                        (for/list ([<enum> (in-value (check-duplicates enums eq? #:key syntax-e))])
                          (when (syntax? <enum>)
                            (raise-syntax-error 'define-enumeration "duplicate name" <enum> #false
                                                (filter (λ [<e>] (and (eq? (syntax-e <e>) (syntax-e <enum>))
                                                                      (not (eq? <e> <enum>))))
                                                        enums)))))])
       (syntax/loc stx
         (begin (define-type TypeU (U 'enum ...))
                (define ids : (Pairof TypeU (Listof TypeU)) (list 'enum ...))
                (define id? : (-> Any Boolean : TypeU) (λ [v] (or (eq? v 'enum) ...))))))]

    [(_ id : TypeU [enum ...])
     (with-syntax ([ids (format-id #'id "~as" (syntax-e #'id))])
       (syntax/loc stx (define-enumeration [id ids] : TypeU [enum ...])))]))

(define-syntax (define-enumeration* stx)
  (syntax-parse stx
    [(_ id #:as TypeU kw-filter #:-> [args Args] ... Type [(enum) sexp ...] ... [#:else defsexp ...])
     (syntax/loc stx
       (begin (define-enumeration id : TypeU [enum ...])
              (define (kw-filter [kw : Symbol] [args : Args] ...) : Type
                (cond [(eq? kw 'enum) sexp ...] ... [else defsexp ...]))))]

    [(_ id #:as TypeU kw-filter #:-> [args Args] ... Type [(enum) sexp ...] ...)
     (syntax/loc stx
       (begin (define-enumeration id : TypeU [enum ...])
              (define kw-filter : (All (a) (case-> [TypeU Args ... -> Type]
                                                   [Symbol Args ... -> (Option Type)]
                                                   [Symbol Args ... (-> Symbol String Symbol a) -> (U Type a)]))
                (let ([expected (exn-constraint->string (list 'enum ...))])
                  (case-lambda
                    [(kw args ... throw) (or (kw-filter kw args ...) (throw 'kw-filter expected kw))]
                    [(kw args ...) (cond [(eq? kw 'enum) sexp ...] ... [else #false])])))))]

    [(_ id #:+> TypeU kw->enum enum->kw [enum:id value:integer] ...)
     (syntax/loc stx
       (begin (define-enumeration id : TypeU #:with kw->enum [enum value] ...)
              (define enum->kw : (All (a) (case-> [Integer -> (Option TypeU)]
                                                  [Integer TypeU -> TypeU]
                                                  [Integer (-> Symbol String Integer a) -> (U TypeU a)]))
                (let ([expected (exn-constraint->string (list value ...))])
                  (case-lambda
                    [(kv throw) (or (enum->kw kv) (if (symbol? throw) throw (throw 'enum->kw expected kv)))]
                    [(kv) (cond [(= kv value) 'enum] ... [else #false])])))))]

    [(_ id #:+> TypeU kw->enum enum->kw [start:integer enum ... enum$])
     (with-syntax* ([(value ... value$) (for/list ([<enum> (in-syntax #'(enum ... enum$))] [idx (in-naturals 0)])
                                          (datum->syntax <enum> (+ (syntax-e #'start) idx) #| `start` might be negative integers |#))])
       (syntax/loc stx
         (begin (define-enumeration id : TypeU #:with kw->enum [enum value] ... [enum$ value$])
                (define enum->kw : (All (a) (case-> [Integer -> (Option TypeU)]
                                                    [Integer TypeU -> TypeU]
                                                    [Integer (-> Symbol String Integer a) -> (U TypeU a)]))
                  (let ([expected (exn-constraint->string (cons start value$))])
                    (case-lambda
                      [(kv throw) (or (enum->kw kv) (if (symbol? throw) throw (throw 'enum->kw expected kv)))]
                      [(kv) (cond [(= kv value) 'enum] ... [(= kv value$) 'enum$] [else #false])]))))))]

    [(_ id #:+> TypeU kw->enum enum->kw #:range [enum value] ... [enum$ value$])
     (with-syntax ([(range ...) (for/list ([<start> (in-syntax #'(value ...))]
                                           [<end> (sequence-tail (in-syntax #'(value ... value$)) 1)])
                                  (datum->syntax <start> (/ (+ (syntax-e <start>) (syntax-e <end>)) 2)))])
       (syntax/loc stx
         (begin (define-enumeration id : TypeU #:with kw->enum [enum value] ... [enum$ value$])
                (define (enum->kw [kv : Integer]) : TypeU
                  (cond [(< kv range) 'enum] ...
                        [else 'enum$])))))]))
