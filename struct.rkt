#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax racket/symbol))

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-configuration stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID #:format frmt:str ([field : FieldType defval] ...))
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                   [([kw-args ...] [default-parameter ...])
                    (let-values ([(args sarap)
                                  (for/fold ([args null] [sarap null])
                                            ([<field> (in-syntax #'(field ...))]
                                             [<FiledType> (in-syntax #'(FieldType ...))])
                                    (define <param> (datum->syntax <field> (string->symbol (format (syntax-e #'frmt) (syntax-e <field>)))))
                                    (define <kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>)))))
                                    (values (cons <kw-name> (cons #`[#,<field> : (Option #,<FiledType>) #false] args))
                                            (cons <param> sarap)))])
                      (list args (reverse sarap)))])
       (syntax/loc stx
         (begin (struct id ([field : FieldType] ...) #:transparent #:type-name ID)

                (define default-parameter : (Parameterof FieldType) (make-parameter defval)) ...
                
                (define (make-id kw-args ...) : ID
                  (id (or field (default-parameter)) ...)))))]))

(define-syntax (define-object stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([method : MethodType defmth ...] ...))
     (syntax/loc stx (define-object id : ID () ([method : MethodType defmth ...] ...)))]
    [(_ id : ID ([field : FieldType defval ...] ...) ([method : MethodType defmth ...] ...))
     (with-syntax* ([ABS-ID (format-id #'id "Abstract-~a" (syntax-e #'ID))]
                    [abs-id (format-id #'id "abstract-~a" (syntax-e #'id))]
                    [id-abs (format-id #'id "~a-interface" (syntax-e #'id))]
                    [make-id (format-id #'id "make-~a" (syntax-e #'id))]
                    [super-id (format-id #'id "super-~a" (syntax-e #'id))]
                    [(kw-args ...)
                     (for/fold ([args null])
                               ([<field> (in-syntax #'(field ... method ...))]
                                [<Argument> (in-syntax #'([field : FieldType defval ...] ... [method : MethodType defmth ...] ...))])
                       (cons (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))
                             (cons <Argument> args)))]
                    [([id-field ...] [abs-field ...])
                     (let-values ([(sdleif sdleif-abs)
                                   (for/fold ([sdleif null] [sdleif-abs null])
                                             ([<field> (in-syntax #'(field ...))])
                                     (define <id-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e <field>)))))
                                     (define <abs-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'abs-id) (syntax-e <field>)))))
                                     (values (cons <id-field> sdleif) (cons <abs-field> sdleif-abs)))])
                       (list (reverse sdleif) (reverse sdleif-abs)))]
                    [([id.method ...] [id-method ...] [abs-method ...])
                     (let-values ([(sylppa sdleif sdleif-abs)
                                   (for/fold ([sylppa null] [sdleif null] [sdleif-abs null])
                                             ([<method> (in-syntax #'(method ...))])
                                     (define <id-apply> (datum->syntax <method> (string->symbol (format "~a.~a" (syntax-e #'id) (syntax-e <method>)))))
                                     (define <id-field> (datum->syntax <method> (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e <method>)))))
                                     (define <abs-field> (datum->syntax <method> (string->symbol (format "~a-~a" (syntax-e #'abs-id) (syntax-e <method>)))))
                                     (values (cons <id-apply> sylppa) (cons <id-field> sdleif) (cons <abs-field> sdleif-abs)))])
                       (list (reverse sylppa) (reverse sdleif) (reverse sdleif-abs)))])
       (syntax/loc stx
         (begin (struct abs-id ([field : FieldType] ... [method : MethodType] ...) #:transparent #:type-name ABS-ID)
                (struct id ([interface : abs-id]) #:type-name ID)

                (define (make-id kw-args ...) : ID
                  (id (abs-id field ... method ...)))
                
                (define (super-id kw-args ...) : ABS-ID
                  (abs-id field ... method ...))

                (define (id-field [self : ID]) : FieldType
                  (abs-field (id-abs self)))
                ...
                                             
                (define (id-method [self : ID]) : MethodType
                  (abs-method (id-abs self)))
                ...
                                              
                (define-syntax (id.method stx)
                  (syntax-case stx []
                    [(_ self-expr argl [... ...])
                     #'(let ([self self-expr]) ((abs-method (id-abs self)) self argl [... ...]))]))
                ...)))]))
