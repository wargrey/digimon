#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))

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
                                    (define <kw-name> (datum->syntax <field> (string->keyword (symbol->string (syntax-e <field>)))))      
                                    (values (cons <kw-name> (cons #`[#,<field> : (Option #,<FiledType>) #false] args))
                                            (cons <param> sarap)))])
                      (list args (reverse sarap)))])
       #'(begin (struct id ([field : FieldType] ...) #:transparent #:type-name ID)

                (define default-parameter : (Parameterof FieldType) (make-parameter defval)) ...
                
                (define (make-id kw-args ...) : ID
                  (id (or field (default-parameter)) ...))))]))

(define-syntax (define-object stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([method : MethodType defmth ...] ...))
     #'(define-object id : ID () ([method : MethodType defmth ...] ...))]
    [(_ id : ID ([field : FieldType defval ...] ...) ([method : MethodType defmth ...] ...))
     (with-syntax* ([api-id (datum->syntax #'id (gensym (format "~a$" (syntax-e #'id))))]
                    [id-api (format-id #'id "~a-~a" (syntax-e #'id) (syntax-e #'api-id))]
                    [make-id (format-id #'id "make-~a" (syntax-e #'id))]
                    [(kw-args ...)
                     (for/fold ([args null])
                               ([<field> (in-syntax #'(field ... method ...))]
                                [<Argument> (in-syntax #'([field : FieldType defval ...] ... [method : MethodType defmth ...] ...))])
                       (cons (datum->syntax <field> (string->keyword (symbol->string (syntax-e <field>))))
                             (cons <Argument> args)))]
                    [([id-field ...] [api-field ...])
                     (let-values ([(sdleif sdleif-api)
                                   (for/fold ([sdleif null] [sdleif-api null])
                                             ([<field> (in-syntax #'(field ...))])
                                     (define <id-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e <field>)))))
                                     (define <api-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'api-id) (syntax-e <field>)))))
                                     (values (cons <id-field> sdleif) (cons <api-field> sdleif-api)))])
                       (list (reverse sdleif) (reverse sdleif-api)))]
                    [([id-method-apply ...] [id-method ...] [api-method ...])
                     (let-values ([(sylppa sdleif sdleif-api)
                                   (for/fold ([sylppa null] [sdleif null] [sdleif-api null])
                                             ([<method> (in-syntax #'(method ...))])
                                     (define <id-apply> (datum->syntax <method> (string->symbol (format "~a.~a" (syntax-e #'id) (syntax-e <method>)))))
                                     (define <id-field> (datum->syntax <method> (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e <method>)))))
                                     (define <api-field> (datum->syntax <method> (string->symbol (format "~a-~a" (syntax-e #'api-id) (syntax-e <method>)))))
                                     (values (cons <id-apply> sylppa) (cons <id-field> sdleif) (cons <api-field> sdleif-api)))])
                       (list (reverse sylppa) (reverse sdleif) (reverse sdleif-api)))])
       #'(begin (struct api-id ([field : FieldType] ... [method : MethodType] ...))
                (struct id ([api-id : api-id]) #:type-name ID)

                (define (make-id kw-args ...) : ID
                  (id (api-id field ... method ...)))

                (define (id-field [self : ID]) : FieldType
                  (api-field (id-api self)))
                ...
                                             
                (define (id-method [self : ID]) : MethodType
                  (api-method (id-api self)))
                ...
                                              
                (define-syntax (id-method-apply stx)
                  (syntax-case stx []
                    [(_ self argl [... ...])
                     #'((api-method (id-api self)) argl [... ...])]))
                ...))]))
