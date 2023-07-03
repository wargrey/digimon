#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax typed/racket/base))

(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bitmask stx)
  (syntax-case stx [:]
    [(_ id : TypeU #:with kw->enum [enum value] ...)
     (with-syntax ([EnumType (let ([mvalue (apply max (syntax->datum #'(value ...)))])
                               (cond [(byte? mvalue) #'Byte]
                                     [(index? mvalue) #'Index]
                                     [(fixnum? mvalue) #'Nonnegative-Fixnum]
                                     [else #'Natural]))])
       (syntax/loc stx
         (begin (define-bitmask id : TypeU [enum ...])
                
                (define kw->enum : (case-> [TypeU -> (U value ...)]
                                           [Symbol -> (U False value ...)]
                                           [(Listof Symbol) -> EnumType])
                  (lambda [kw]
                    (if (list? kw)
                        (let fold ([sum : EnumType 0]
                                   [rst : (Listof Symbol) kw])
                          (if (pair? rst)
                              (let ([v (kw->enum (car rst))])
                                (fold (if v (bitwise-ior sum v) sum) (cdr rst)))
                              sum))
                        (cond [(eq? kw 'enum) value] ...
                              [else #false])))))))]

    [(_ [id ids] : TypeU [enum ...])
     (with-syntax ([id? (format-id #'id "~a?" (syntax-e #'id))]
                   [_ (let ([enums (syntax->list #'[enum ...])])
                        (for/list ([<enum> (in-value (check-duplicates enums eq? #:key syntax-e))])
                          (when (syntax? <enum>)
                            (raise-syntax-error 'define-bitmask "duplicate name" <enum> #false
                                                (filter (λ [<e>] (and (eq? (syntax-e <e>) (syntax-e <enum>))
                                                                      (not (eq? <e> <enum>))))
                                                        enums)))))])
       (syntax/loc stx
         (begin (define-type TypeU (U 'enum ...))
                (define ids : (Pairof TypeU (Listof TypeU)) (list 'enum ...))
                (define id? : (-> Any Boolean : TypeU) (λ [v] (or (eq? v 'enum) ...))))))]

    [(_ id : TypeU [enum ...])
     (with-syntax ([ids (format-id #'id "~as" (syntax-e #'id))])
       (syntax/loc stx (define-bitmask [id ids] : TypeU [enum ...])))]))

(define-syntax (define-bitmask* stx)
  (syntax-parse stx
    [(_ id #:+> TypeU kw->enum enum->kw [enum:id value:integer] ...)
     (with-syntax ([EnumType (let ([mvalue (apply max (syntax->datum #'(value ...)))])
                               (cond [(byte? mvalue) #'Byte]
                                     [(index? mvalue) #'Index]
                                     [(fixnum? mvalue) #'Nonnegative-Fixnum]
                                     [else #'Natural]))]
                   [([renum rvalue] ...) (for/fold ([rs null])
                                                   ([<e> (syntax->list #'(enum ...))]
                                                    [<v> (syntax->list #'(value ...))])
                                           (when (zero? (syntax-e <v>))
                                             (raise-syntax-error 'define-bitmask* "zero value"
                                                                 <e> #false (list <v>)))
                                           (cons (list <e> <v>) rs))])
       (syntax/loc stx
         (begin (define-bitmask id : TypeU #:with kw->enum [enum value] ...)
                (define enum->kw : (-> Natural (Listof TypeU))
                  (lambda [n]
                    (cond [(= n 0) null]
                          [else (let ->kw ([enms : (Listof TypeU) (list 'renum ...)]
                                           [vals : (Listof EnumType) (list rvalue ...)]
                                           [kws : (Listof TypeU) null])
                                  (if (and (pair? enms) (pair? vals))
                                      (let ([v (car vals)])
                                        (if (= v (bitwise-and v n))
                                            (->kw (cdr enms) (cdr vals) (cons (car enms) kws))
                                            (->kw (cdr enms) (cdr vals) kws)))
                                      kws))]))))))]

    [(_ id #:+> TypeU kw->enum enum->kw [start:integer enum ... enum$])
     (with-syntax* ([(value ... value$) (for/list ([<enum> (in-syntax #'(enum ... enum$))]
                                                   [idx (in-naturals (integer-length (syntax-e #'start)))])
                                          (datum->syntax <enum> (arithmetic-shift 1 idx)))])
       (syntax/loc stx
         (define-bitmask* id #:+> TypeU kw->enum enum->kw [enum value] ... [enum$ value$])))]))
