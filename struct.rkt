#lang typed/racket/base

(provide (all-defined-out))

(require "syntax.rkt")

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-configuration stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID #:as parent #:format frmt:str ([field : FieldType defval] ...))
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                   [([kw-args ...] [default-parameter ...])
                    (let-values ([(args sarap)
                                  (for/fold ([args null] [sarap null])
                                            ([<field> (in-syntax #'(field ...))]
                                             [<FiledType> (in-syntax #'(FieldType ...))])
                                    (define <param> (datum->syntax <field> (string->symbol (format (syntax-e #'frmt) (syntax-e <field>)))))
                                    (define <kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>)))))
                                    (values (cons <kw-name> (cons #`[#,<field> : #,<FiledType> ((#,<param>))] args))
                                            (cons <param> sarap)))])
                      (list args (reverse sarap)))])
       (syntax/loc stx
         (begin (struct id parent () #:type-name ID #:transparent)

                (define default-parameter : (Parameterof FieldType (-> FieldType))
                  ((inst make-parameter FieldType (-> FieldType)) (λ [] defval) (λ [[v : FieldType]] (λ [] v))))
                ...

                (define (make-id kw-args ...) : ID
                  (id field ...)))))]
    [(_ id : ID #:-> parent #:format frmt:str ([field : FieldType defval] ...))
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                   [([kw-args ...] [default-parameter ...])
                    (let-values ([(args sarap)
                                  (for/fold ([args null] [sarap null])
                                            ([<field> (in-syntax #'(field ...))]
                                             [<FiledType> (in-syntax #'(FieldType ...))])
                                    (define <param> (datum->syntax <field> (string->symbol (format (syntax-e #'frmt) (syntax-e <field>)))))
                                    (define <kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>)))))
                                    (values (cons <kw-name> (cons #`[#,<field> : #,<FiledType> ((#,<param>))] args))
                                            (cons <param> sarap)))])
                      (list args (reverse sarap)))])
       (syntax/loc stx
         (begin (struct id parent ([field : FieldType] ...) #:type-name ID #:transparent)

                (define default-parameter : (Parameterof FieldType (-> FieldType))
                  ((make-parameter FieldType (-> FieldType)) (λ [] defval) (λ [[v : FieldType]] (λ [] v))))
                ...
                
                (define (make-id kw-args ...) : ID
                  (id field ...)))))]
    [(_ id : ID #:format frmt:str ([field : FieldType defval] ...))
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                   [([kw-args ...] [default-parameter ...])
                    (let-values ([(args sarap)
                                  (for/fold ([args null] [sarap null])
                                            ([<field> (in-syntax #'(field ...))]
                                             [<FiledType> (in-syntax #'(FieldType ...))])
                                    (define <param> (datum->syntax <field> (string->symbol (format (syntax-e #'frmt) (syntax-e <field>)))))
                                    (define <kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>)))))
                                    (values (cons <kw-name> (cons #`[#,<field> : #,<FiledType> ((#,<param>))] args))
                                            (cons <param> sarap)))])
                      (list args (reverse sarap)))])
       (syntax/loc stx
         (begin (struct id ([field : FieldType] ...) #:type-name ID #:transparent)

                (define default-parameter : (Parameterof FieldType (-> FieldType))
                  ((inst make-parameter FieldType (-> FieldType)) (λ [] defval) (λ [[v : FieldType]] (λ [] v))))
                ...
                
                (define (make-id kw-args ...) : ID
                  (id field ...)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-object stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([method : MethodType defmth ...] ...))
     (syntax/loc stx (define-object id : ID () ([method : MethodType defmth ...] ...)))]
    [(_ id : ID ([field : FieldType defval ...] ...) ([method : MethodType defmth ...] ...))
     (with-syntax* ([ABS-ID (format-id #'ID "Abstract-~a" (syntax-e #'ID))]
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
                (struct id ([interface : ABS-ID]) #:type-name ID)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-struct stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([field : FieldType defval ...] ...) options ...)
     (with-syntax* ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                    [remake-id (format-id #'id "remake-~a" (syntax-e #'id))]
                    [(field-ref ...) (make-identifiers #'id #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (define-type ID id)
                (struct id ([field : FieldType] ...) options ...)

                (define (make-id kw-args ...) : ID
                  (id field ...))

                (define (remake-id [self : ID] kw-reargs ...) : ID
                  (id (if (void? field) (field-ref self) field) ...)))))]
    [(_ sid : SID #:-> super
        (~optional (~seq #:head [[hid hfield : HFieldType hdefval ...] ...])
                   #:defaults ([(hid 1) null] [(hfield 1) null] [(HFieldType 1) null] [(hdefval 2) null]))
        ([field : FieldType defval ...] ...)
        (~optional (~seq #:substruct [(deftree sub : Sub subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(sub 1) null] [(Sub 1) null] [(subrest 2) null]))
        options ...)
     (with-syntax* ([make-id (format-id #'sid "make-~a" (syntax-e #'sid))]
                    [remake-id (format-id #'sid "remake-~a" (syntax-e #'sid))]
                    [derive-id (format-id #'sid "derive-~a" (syntax-e #'sid))]
                    [(field-ref ...) (make-identifiers #'sid #'(field ...))]
                    [(hfield-ref ...) (for/list ([<hid> (in-syntax #'(hid ...))]
                                                 [<hfield> (in-syntax #'(hfield ...))])
                                        (format-id <hfield> "~a-~a" (syntax-e <hid>) (syntax-e <hfield>)))]
                    [([hkw-args ...] [hkw-reargs ...]) (make-keyword-arguments #'(hfield ...) #'(HFieldType ...) #'([hdefval ...] ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (define-type SID sid)
                (struct sid super ([field : FieldType] ...) options ...)

                (define (make-id hkw-args ... kw-args ...) : SID
                  (sid hfield ... field ...))

                (define (remake-id [self : SID] hkw-reargs ... kw-reargs ...) : SID
                  (sid (if (void? hfield) (hfield-ref self) hfield) ...
                       (if (void? field) (field-ref self) field) ...))

                (define (derive-id [self : super] hkw-reargs ... kw-args ...) : SID
                  (sid (if (void? hfield) (hfield-ref self) hfield) ...
                       field ...))

                (deftree sub : Sub #:-> sid
                  #:head [[hid hfield : HFieldType hdefval ...] ...
                          [sid field : FieldType defval ...] ...]
                  subrest ...) ...)))]))

(define-syntax (define-struct/parameter stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([field : FieldType defval ...] ...) options ...)
     (with-syntax* ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                    [remake-id (format-id #'id "remake-~a" (syntax-e #'id))]
                    [default-id (format-id #'id "default-~a" (syntax-e #'id))]
                    [(field-ref ...) (make-identifiers #'id #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (define-type ID id)
                (struct id ([field : FieldType] ...) options ...)

                (define (make-id kw-args ...) : ID
                  (id field ...))
                
                (define (remake-id [self : ID (default-id)] kw-reargs ...) : ID
                  (id (if (void? field) (field-ref self) field) ...))

                (define default-id : (Parameterof ID) (make-parameter (make-id))))))]))

(define-syntax (define-struct* stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([field : FieldType defval ...] ...) options ...)
     (with-syntax* ([id-apply (format-id #'id "~a-apply" (syntax-e #'id))]
                    #;[id-copy (format-id #'id "~a-copy" (syntax-e #'id))]
                    [(field-ref ...) (make-identifiers #'id #'(field ...))]
                    [(kw-field ...) (make-identifier-keywords #'(field ...))])
       (syntax/loc stx
         (begin (define-struct id : ID ([field : FieldType defval ...] ...) options ...)

                (define-syntax (id-apply nstx)
                  (syntax-parse nstx #:literals [:]
                    [(_ f self
                        (~alt (~optional (~seq kw-field field) #:defaults ([field #'(void)])) ...) [... ...]
                        argl [... ...])
                     (syntax/loc nstx
                       (f (if (void? field) (field-ref self) field) ...
                          argl [... ...]))]))

                ; TODO: It causes "parent struct info not known" out of the module defining the struct
                #;(define-syntax (id-copy nstx)
                  (syntax-case nstx [:]
                    [(_ sub self ([self-field datum] [... ...]) [sub-field subdatum] [... ...])
                     (syntax/loc nstx
                       (struct-copy sub self
                                    [self-field #:parent id datum] [... ...]
                                    [sub-field subdatum] [... ...]))])))))]
    [(_ sid : SID #:-> super
        (~optional (~seq #:head [[hid hfield : HFieldType hdefval ...] ...])
                   #:defaults ([(hid 1) null] [(hfield 1) null] [(HFieldType 1) null] [(hdefval 2) null]))
        ([field : FieldType defval ...] ...)
        (~optional (~seq #:substruct [(deftree sub : Sub subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(sub 1) null] [(Sub 1) null] [(subrest 2) null]))
        options ...)
     (with-syntax* ([id-apply (format-id #'sid "~a-apply" (syntax-e #'sid))]
                    #;[id-copy (format-id #'sid "~a-copy" (syntax-e #'sid))]
                    [(kw-hfield ...) (make-identifier-keywords #'(hfield ...))]
                    [(kw-field ...) (make-identifier-keywords #'(field ...))]
                    [(field-ref ...) (make-identifiers #'sid #'(field ...))]
                    [(hfield-ref ...) (for/list ([<hid> (in-syntax #'(hid ...))]
                                                 [<hfield> (in-syntax #'(hfield ...))])
                                        (format-id <hfield> "~a-~a" (syntax-e <hid>) (syntax-e <hfield>)))])
       (syntax/loc stx
         (begin (define-struct sid : SID #:-> super
                  #:head [[hid hfield : HFieldType hdefval ...] ...]
                  ([field : FieldType defval ...] ...) options ...)
               
                (define-syntax (id-apply nstx)
                  (syntax-parse nstx #:literals [:]
                    [(_ f self
                        (~alt (~optional (~seq kw-hfield hfield) #:defaults ([hfield #'(void)])) ...
                              (~optional (~seq kw-field field) #:defaults ([field #'(void)])) ...) [... ...]
                        argl [... ...])
                     (syntax/loc nstx
                       (f (if (void? hfield) (hfield-ref self) hfield) ...
                          (if (void? field) (field-ref self) field) ...
                          argl [... ...]))]))

                ; It causes "parent struct info not known" out of the module defining the struct
                #;(define-syntax (id-copy nstx)
                  (syntax-case nstx [:]
                    [(_ sub self ([self-field datum] [... ...]) [sub-field subdatum] [... ...])
                     (syntax/loc nstx
                       (struct-copy sub self
                                    [self-field #:parent id datum] [... ...]
                                    [sub-field subdatum] [... ...]))]))

                (deftree sub : Sub #:-> sid
                  #:head [[hid hfield : HFieldType hdefval ...] ...
                          [sid field : FieldType defval ...] ...]
                  subrest ...) ...)))]))

(define-syntax (define-structs stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([hfield : HFieldType hdefval ...] ...)
        (~optional (~seq #:substruct [(deftree sub : Sub subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(sub 1) null] [(Sub 1) null] [(subrest 2) null]))
        options ...)
     (syntax/loc stx
       (begin (define-struct id : ID ([hfield : HFieldType hdefval ...] ...) options ...)
              
              (deftree sub : Sub #:-> id
                #:head [[id hfield : HFieldType hdefval ...] ...]
                subrest ...) ...))]))

(define-syntax (define-structs* stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID ([hfield : HFieldType hdefval ...] ...)
        (~optional (~seq #:substruct [(deftree sub : Sub subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(sub 1) null] [(Sub 1) null] [(subrest 2) null]))
        options ...)
     (syntax/loc stx
       (begin (define-struct* id : ID ([hfield : HFieldType hdefval ...] ...) options ...)
              
              (deftree sub : Sub #:-> id
                #:head [[id hfield : HFieldType hdefval ...] ...]
                subrest ...) ...))]))
