#lang typed/racket/base

(provide (all-defined-out) drop-bytes)
(provide (rename-out [write-muintptr write-msize]
                     [write-luintptr write-lsize]))

(require racket/list)

(require "port.rkt")
(require "string.rkt")
(require "character.rkt")

(require "digitama/ioexn.rkt")
(require "digitama/stdio.rkt")

(require "digitama/unsafe/number.rkt")
(require "digitama/unsafe/ops.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax syntax/parse))
(require (for-syntax racket/symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0xFF16 : Index #xFFFF)
(define 0xFF32 : Index (assert #xFFFFFFFF index?))
(define 0xFF31 : Index (assert #x7FFFFFFF index?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Subbytes (List Bytes Index Index))
(define-type Octets (U Bytes Subbytes))

(begin-for-syntax
  (struct bintype (type fixed-size read-int write-int dynamic-size datum->raw raw->datum) #:prefab)
  (struct fieldinfo (default signature? omittable? peek-int name radix fixed-offset) #:prefab)

  (define (stdio-integer-type/read->peek <read-int>)
    (case (syntax-e <read-int>)
      [(read-luint8)  #'peek-luint8]
      [(read-msint16) #'peek-msint16]
      [(read-muint16) #'peek-muint16]
      [(read-lsint16) #'peek-lsint16]
      [(read-luint16) #'peek-luint16]
      [(read-msint32) #'peek-msint32]
      [(read-muint32) #'peek-muint32]
      [(read-lsint32) #'peek-lsint32]
      [(read-luint32) #'peek-luint32]
      [(read-msint64) #'peek-msint64]
      [(read-muint64) #'peek-muint64]
      [(read-lsint64) #'peek-lsint64]
      [(read-luint64) #'peek-luint64]
      [else <read-int>]))

  (define (make-bintype type word-size read-int write-int dynamic-size [datum->raw null] [raw->datum null])
    (bintype type word-size read-int write-int dynamic-size datum->raw raw->datum))

  (define (remake-bintype bt #:type [type #false] #:fixed-size [fixed-size #false]
                          #:read [read-int #false] #:write [write-int #false]
                          #:dynamic-size [dynamic-size #false]
                          #:datum->raw [datum->raw #false] #:raw->datum [raw->datum #false])
    (bintype (or type (bintype-type bt)) (or fixed-size (bintype-fixed-size bt))
             (or read-int (bintype-read-int bt)) (or write-int (bintype-write-int bt))
             (or dynamic-size (bintype-dynamic-size bt))
             (or datum->raw (bintype-datum->raw bt)) (or raw->datum (bintype-raw->datum bt)))))

(define-for-syntax (stdio-integer-type datatype)
  (case datatype
    [(Byte Octet UInt8)          (make-bintype #'Byte    1 #'read-luint8  #'write-msintptr #'stdio-zero-size)]
    [(Short MShort Int16 MInt16) (make-bintype #'Fixnum  2 #'read-msint16 #'write-msintptr #'stdio-zero-size)]
    [(UInt16 MUInt16)            (make-bintype #'Index   2 #'read-muint16 #'write-muintptr #'stdio-zero-size)]
    [(LShort LInt16)             (make-bintype #'Fixnum  2 #'read-lsint16 #'write-lsintptr #'stdio-zero-size)]
    [(LUInt16)                   (make-bintype #'Index   2 #'read-luint16 #'write-luintptr #'stdio-zero-size)]
    [(Int MInt Int32 MInt32)     (make-bintype #'Fixnum  4 #'read-msint32 #'write-msintptr #'stdio-zero-size)]
    [(UInt32 MUInt32)            (make-bintype #'Index   4 #'read-muint32 #'write-muintptr #'stdio-zero-size)]
    [(LInt LInt32)               (make-bintype #'Fixnum  4 #'read-lsint32 #'write-lsintptr #'stdio-zero-size)]
    [(LUInt32)                   (make-bintype #'Index   4 #'read-luint32 #'write-luintptr #'stdio-zero-size)]
    [(Long MLong Int64 MInt64)   (make-bintype #'Integer 8 #'read-msint64 #'write-msintptr #'stdio-zero-size)]
    [(UInt64 MUInt64)            (make-bintype #'Natural 8 #'read-muint64 #'write-muintptr #'stdio-zero-size)]
    [(LLong LInt64)              (make-bintype #'Integer 8 #'read-lsint64 #'write-lsintptr #'stdio-zero-size)]
    [(LUInt64)                   (make-bintype #'Natural 8 #'read-luint64 #'write-luintptr #'stdio-zero-size)]
    [else #false]))

(define-for-syntax (stdio-float-type datatype)
  (case datatype
    ;[(Float MFloat)              (list #'flonum 4)]
    ;[(LFloat)                    (list #'flonum 4)]
    ;[(Double MDouble)            (list #'flonum 8)]
    ;[(LDouble)                   (list #'flonum 8)]
    [else #false]))

;; TODO
; `default-stdout-locale` might cause inconsistent values
(define-for-syntax (stdio-bytes-type datatype <fields>)
  (when (null? (cdr datatype))
    (raise-syntax-error 'stdio-bytes-type "needs a parameter, either a field name or a size"
                        datatype))
  
  (case (syntax-e (car datatype))
    [(Bytesof)          (make-bintype #'Bytes  (stdio-target-field (cadr datatype) <fields>) #'read-nbytes      #'write-nbytes      #'bytes-length)]
    [(Stringof)         (make-bintype #'String (stdio-target-field (cadr datatype) <fields>) #'read-nbstring    #'write-nbstring    #'string-utf-8-length)]
    [(Localeof)         (make-bintype #'String (stdio-target-field (cadr datatype) <fields>) #'read-nlcstring   #'write-nlcstring   #'string-locale-length)]

    [(NBytes MNBytes)   (make-bintype #'Bytes  (stdio-word-size (cadr datatype))             #'read-mn:bytes    #'write-mn:bytes    #'bytes-length)]
    [(LNBytes)          (make-bintype #'Bytes  (stdio-word-size (cadr datatype))             #'read-ln:bytes    #'write-ln:bytes    #'bytes-length)]

    [(NString MNString) (make-bintype #'String (stdio-word-size (cadr datatype))             #'read-mn:bstring  #'write-mn:bstring  #'string-utf-8-length)]
    [(LNString)         (make-bintype #'String (stdio-word-size (cadr datatype))             #'read-ln:bstring  #'write-ln:bstring  #'string-utf-8-length)]
    
    [(NLocale MNLocale) (make-bintype #'String (stdio-word-size (cadr datatype))             #'read-mn:lcstring #'write-mn:lcstring #'string-locale-length)]
    [(LNLocale)         (make-bintype #'String (stdio-word-size (cadr datatype))             #'read-ln:lcstring #'write-ln:lcstring #'string-locale-length)]

    [else #false]))

(define-for-syntax (stdio-datum-type <DataType> <layout> <field>)
  (syntax-parse <DataType> #:datum-literals []
    [(#:enum type datum->raw raw->datum (~alt (~optional (~seq #:-> Type)
                                                         #:defaults ([Type #'Symbol]))
                                              (~optional (~seq #:fallback enum)
                                                         #:defaults ([enum #'throw-range-error*]))) ...)
     (let ([maybe-bt (stdio-integer-type (syntax-e #'type))])
       (and (bintype? maybe-bt)
            (remake-bintype maybe-bt #:type #'Type
                            #:datum->raw (list #'datum->raw #'enum)
                            #:raw->datum (list #'raw->datum #'enum))))]
    [(#:enum type datum-identity (~optional (~seq #:fallback enum)
                                            #:defaults ([enum #'throw-range-error*])))
     (let ([maybe-bt (stdio-integer-type (syntax-e #'type))])
       (and (bintype? maybe-bt)
            (remake-bintype maybe-bt #:raw->datum (list #'datum->identity #'enum))))]
    [(#:subint type subint? (~alt (~optional (~seq #:-> Type)
                                             #:defaults ([Type #'#false]))
                                  (~optional (~seq #:throw throw)
                                             #:defaults ([throw #'throw-range-error]))) ...)
     (let ([maybe-bt (stdio-integer-type (syntax-e #'type))])
       (and (bintype? maybe-bt)
            (remake-bintype maybe-bt #:type (and (syntax-e #'Type) #'Type)
                    #:raw->datum (list (list #'subint?)
                                       (format-id #'subint? "~a-~a" (syntax-e <layout>) (syntax-e <field>))
                                       #'throw))))]
    [(#:bitmask type datum->raw raw->datum (~optional (~seq #:-> Type)
                                                      #:defaults ([Type #'(Listof Symbol)])))
     (let ([maybe-bt (stdio-integer-type (syntax-e #'type))])
       (and (bintype? maybe-bt)
            (remake-bintype maybe-bt #:type #'Type
                            #:datum->raw (list #'datum->raw) #:raw->datum (list #'raw->datum))))]

    [(#:-> Type size datum->raw raw->datum)
     (make-bintype #'Type (stdio-fixed-bytes-size #'size) #'read-nbytes #'write-nbytes #'stdio-zero-size
                   (list #'datum->raw #'size) (list #'raw->datum #'size))]

    [(#:raw size)
     (make-bintype #'Bytes (stdio-fixed-bytes-size #'size) #'read-nbytes #'pad-bytes #'stdio-zero-size
                   (list #'values) (list #'values))]
    
    [((~or #:unused #:reserved #:undefined) size)
     (make-bintype #'Any (stdio-fixed-bytes-size #'size) #'drop-bytes #'pad-bytes #'stdio-zero-size
                   (list #'void) (list #'void))]
    
    [(#:locale size)
     (make-bintype #'String (stdio-fixed-bytes-size #'size) #'read-nlcstring #'pad-nlcstring #'stdio-zero-size
                   (list #'values) (list #'values))]
    
    [_ #false]))

(define-for-syntax (stdio-signature-field <field> <signature?> <defval>)
  (and (syntax-e <signature?>)
       (list <field> (car (syntax-e <defval>)))))

(define-for-syntax (stdio-auto-field-for-size <field> <DataType>)
  (define datatype (syntax-e <DataType>))

  (and (pair? datatype)
       (let ([<target-field> (cadr datatype)])
         (and (identifier? <target-field>)
              (case (syntax-e (car datatype))
                [(Bytesof)  (list <target-field> <field> #'bytes-length)]
                [(Stringof) (list <target-field> <field> #'string-utf-8-length)]
                [(Localeof) (list <target-field> <field> #'string-locale-length)]
                [else #false])))))

(define-for-syntax (stdio-field-metainfo <read> <meta>)
  (syntax-parse <meta> #:datum-literals []
    [((~alt (~optional (~or* (~seq (~and #:signature signature?) defval)
                             (~seq #:default defval)
                             #:name "#:signature or #:default option")
                       #:defaults ([signature? #'#false]
                                   [defval #'#false]))
            (~optional (~and #:omittable omittable?) #:defaults ([omittable? #'#false]))
            (~optional (~seq #:name name) #:defaults ([name #'#false]))
            (~optional (~seq #:radix (~and (~or* 2 8 10 16) radix)) #:defaults ([radix #'#false]))
            (~optional (~seq #:+fixed-size fixed-offset:nat) #:defaults ([fixed-offset #'0])))
      ...)
     (fieldinfo (if (syntax-e #'defval) (list #'defval) null)
                (if (syntax-e #'signature?) #'#true #'#false) (if (syntax-e #'omittable?) #'#true #'#false)
                (if (syntax-e #'omittable?) (stdio-integer-type/read->peek <read>) <read>)
                #'name
                (if (syntax-e #'radix) #'radix (if (syntax-e #'signature?) #'16 #'10))
                #'fixed-offset)]))

(define-syntax (define-binary-struct stx)
  (syntax-case stx [:]
    [(_ layout+super : Layout ([field : DataType metainfo ...] ...) options ...)
     (with-syntax* ([(layout super ...) (let ([id+super (syntax-e #'layout+super)])
                                          (if (symbol? id+super) (list #'layout+super) id+super))]
                    [constructor     (format-id #'layout "~a" (gensym (format "~a:" (syntax-e #'layout))))]
                    [make-layout     (format-id #'layout "make-~a" (syntax-e #'layout))]
                    [remake-layout   (format-id #'layout "remake-~a" (syntax-e #'layout))]
                    [sizeof-layout   (format-id #'layout "sizeof-~a" (syntax-e #'layout))]
                    [offsetof-layout (format-id #'layout "offsetof-~a" (syntax-e #'layout))]
                    [read-layout     (format-id #'layout "read-~a" (syntax-e #'layout))]
                    [write-layout    (format-id #'layout "write-~a" (syntax-e #'layout))]
                    [bytes->layout   (format-id #'layout "bytes->~a" (syntax-e #'layout))]
                    [layout->bytes   (format-id #'layout "~a->bytes" (syntax-e #'layout))]
                    [display-layout  (format-id #'layout "display-~a" (syntax-e #'layout))]
                    [(#s[bintype FieldType fixed-size read-field write-field dynamic-size
                                 [datum->raw ...] [raw->datum ...]] ...)
                     (let ([<fields> #'(field ...)])
                       (for/list ([<DataType> (in-syntax #'(DataType ...))]
                                  [<field> (in-syntax #'(field ...))])
                         (define datatype (syntax-e <DataType>))
                         (or (stdio-integer-type datatype)
                             (stdio-float-type datatype)
                             (and (pair? datatype)
                                  (or (stdio-bytes-type datatype <fields>)
                                      (stdio-datum-type <DataType> #'layout <field>)))
                             (raise-syntax-error 'define-binary-struct "unrecognized data type" <DataType>))))]
                    [(#s[fieldinfo [defval ...] signature? omittable? peek-field
                                   display-name display-radix fixed-offset] ...)
                     (for/list ([<metainfo> (in-syntax #'([metainfo ...] ...))]
                                [<read> (in-syntax #'(read-field ...))])
                       (stdio-field-metainfo <read> <metainfo>))]
                    [([sig-field magic-number] ...)
                     (filter list?
                             (for/list ([<field> (in-syntax #'(field ...))]
                                        [<signature?> (in-syntax #'(signature? ...))]
                                        [<defval> (in-syntax #'([defval ...] ...))])
                               (stdio-signature-field <field> <signature?> <defval>)))]
                    [([auto-field target-field field->value] ...)
                     (filter list?
                             (for/list ([<field> (in-syntax #'(field ...))]
                                        [<DataType> (in-syntax #'(DataType ...))])
                               (stdio-auto-field-for-size <field> <DataType>)))]
                    [([field-ref auto?] ...)
                     (let ([autofields (syntax->datum #'(auto-field ...))])
                       (for/list ([<field> (in-syntax #'(field ...))])
                         (define field (syntax-e <field>))
                         (list (format-id <field> "~a-~a" (syntax-e #'layout) field)
                               (and (memq field autofields) #true))))]
                    [([kw-args ...] [kw-reargs ...] [(man-field man-ref) ...] [auto-fixed-offset ...])
                     (let*-values ([(sig-fields) (syntax->datum #'(sig-field ...))]
                                   [(auto-fields) (syntax->datum #'(auto-field ...))])
                       (for/fold ([args null]
                                  [reargs null]
                                  [sdleif null]
                                  [stesffo null]
                                  #:result (list args reargs (reverse sdleif) (reverse stesffo)))
                                 ([<field> (in-syntax #'(field ...))]
                                  [<ref> (in-syntax #'(field-ref ...))]
                                  [<fixed-offset> (in-syntax #'(fixed-offset ...))]
                                  [<Argument> (in-syntax #'([field : FieldType defval ...] ...))]
                                  [<ReArgument> (in-syntax #'([field : (Option FieldType) #false] ...))])
                         (define field (syntax-e <field>))
                         (cond [(memq field sig-fields)
                                (stdio-check-fixed-offset <fixed-offset>)
                                (values args reargs sdleif stesffo)]
                               [(memq field auto-fields)
                                (values args reargs sdleif (cons <fixed-offset> stesffo))]
                               [else
                                (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string field)))])
                                  (stdio-check-fixed-offset <fixed-offset>)
                                  (values (cons <kw-name> (cons <Argument> args))
                                          (cons <kw-name> (cons <ReArgument> reargs))
                                          (cons (list <field> <ref>) sdleif)
                                          stesffo))])))]
                    [(size0 [offset field-n] ...)
                     (for/fold ([size0 0] [offsets null] #:result (cons size0 offsets))
                               ([size (in-list (reverse (map syntax-e (syntax->list #'(fixed-size ...)))))]
                                [n (in-naturals 0)])
                       (values (cond [(not (exact-integer? size)) size0]
                                     [(>= size 0) (+ size0 size)]
                                     [else (- size0 size)])
                               (cons (list size0 n) offsets)))])
       (syntax/loc stx
         (begin (struct layout super ... ([field : FieldType] ...)
                  #:constructor-name constructor
                  #:type-name Layout
                  #:property prop:custom-write
                  (λ [[self : Layout] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
                    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
                    (display (if (eq? mode 0) "(struct:" "#<") /dev/stdout)
                    (display 'layout /dev/stdout)
                    (for ([datum (in-list (list (field-ref self) ...))]
                          [width (in-list (list 'fixed-size ...))]
                          [radix (in-list (list display-radix ...))])
                      (display #\space /dev/stdout)
                      (stdio-write-field datum width radix write-datum /dev/stdout))
                    (write-char (if (eq? mode 0) #\) #\>) /dev/stdout)
                    (flush-output /dev/stdout))
                  #:transparent
                  options ...)

                (define (make-layout kw-args ...) : Layout
                  (let ([sig-field magic-number] ...
                        [auto-field (unsafe-idx+ (field->value target-field) auto-fixed-offset)] ...)
                    (constructor field ...)))

                (define (remake-layout [src : Layout] kw-reargs ...) : Layout
                  (let* ([sig-field magic-number] ...
                         [man-field (or man-field (man-ref src))] ...
                         [auto-field (unsafe-idx+ (field->value target-field) auto-fixed-offset)] ...)
                    (constructor field ...)))

                (define sizeof-layout : (case-> [-> Index]
                                                [Layout -> Natural])
                  (case-lambda
                    [() size0]
                    [(instance) (+ size0 (dynamic-size (field-ref instance)) ...)]))

                (define offsetof-layout : (case-> [Symbol -> Index]
                                                  [Layout Symbol -> Natural])
                  (case-lambda
                    [(fieldname)
                     (case fieldname
                       [(field) offset] ...
                       [else (raise-argument-error 'offsetof-layout
                                                   (exn-constraint->string '(field ...))
                                                   fieldname)])]
                    [(instance fieldname)
                     (let ([sizes (list (dynamic-size (field-ref instance)) ...)])
                       (case fieldname
                         [(field) (apply + offset (take sizes field-n))] ...
                         [else (raise-argument-error 'offsetof-layout
                                                     (exn-constraint->string '(field ...))
                                                     1 instance fieldname)]))]))

                (define read-layout : (->* () (Input-Port (Option Integer)) Layout)
                  (let ([sizes : (HashTable Symbol Index) (make-hasheq)])
                    (lambda [[/dev/stdin (current-input-port)] [posoff #false]]
                      (unless (not posoff)
                        (port-seek /dev/stdin posoff))

                      (let* ([field (call-datum-reader*
                                     [signature? omittable? peek-field /dev/stdin read-layout defval ...]
                                     [raw->datum ...]
                                     read-field fixed-size fixed-offset /dev/stdin 'field sizes auto?)] ...)
                        (constructor field ...)))))

                (define write-layout : (->* (Layout) (Output-Port (Option Natural)) Natural)
                  (lambda [src [/dev/stdout (current-output-port)] [posoff #false]]
                    (when (exact-nonnegative-integer? posoff)
                      (file-position /dev/stdout posoff))

                    (let* ([sig-field magic-number] ...
                           [man-field (man-ref src)] ...
                           [auto-field (unsafe-idx+ (field->value target-field) auto-fixed-offset)] ...)
                      (+ (call-datum-writer* omittable? (default-stdout-all-fields?)
                                             [datum->raw ...]
                                             write-field field fixed-size /dev/stdout)
                         ...))))

                (define bytes->layout : (->* (Bytes) (Natural) Layout)
                  (lambda [bs [posoff 0]]
                    (let ([/dev/stdin (open-input-memory bs posoff)])
                      (begin0 (read-layout /dev/stdin)
                              (close-input-port /dev/stdin)))))
                
                (define layout->bytes : (case-> [-> Layout Bytes] [->* (Layout Bytes) (Natural) Natural])
                  (case-lambda
                    [(src) (let ([bs (make-bytes (sizeof-layout src))]) (layout->bytes src bs 0) bs)]
                    [(src bs) (layout->bytes src bs 0)]
                    [(src bs off)
                     (let ([/dev/stdout (open-output-memory bs off)])
                       (begin0 (+ off (write-layout src /dev/stdout))
                               (close-output-port /dev/stdout)))]))
                
                (define display-layout : (->* (Layout)
                                              (Output-Port #:mode (U Zero One Boolean)
                                                           #:with-offset? Boolean #:hex-offset? Boolean
                                                           #:hide-void? Boolean)
                                              Void)
                  (lambda [#:mode [mode 1] #:with-offset? [offset? #false] #:hex-offset? [hex? #true]
                           #:hide-void? [hide-void? #true]
                           self [/dev/stdout (current-output-port)]]
                    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
                    (display (stdio-field->name 'layout) /dev/stdout)
                    (unless (not offset?)
                      (display " [sizeof: " /dev/stdout)
                      (display (sizeof-layout self))
                      (display #\] /dev/stdout))
                    (newline /dev/stdout)
                    (let* ([sig-field magic-number] ...
                           [man-field (man-ref self)] ...
                           [auto-field (unsafe-idx+ (field->value target-field) auto-fixed-offset)] ...)
                      (for ([fname (in-list (list (or display-name (stdio-field->name 'field)) ...))]
                            [fnraw (in-list (list 'field ...))]
                            [datum (in-list (list field ...))]
                            [dorig (in-list (list (field-ref self) ...))]
                            [width (in-list (list 'fixed-size ...))]
                            [radix (in-list (list display-radix ...))]
                            [doffs (in-list (list fixed-offset ...))])
                        (unless (and (void? dorig) hide-void?)
                          (display "    " /dev/stdout)
                          (unless (not offset?)
                            (display "[" /dev/stdout)
                            (let ([pos (offsetof-layout self fnraw)])
                              (cond [(not hex?) (display pos /dev/stdout)]
                                    [else (display (string-append "#x" (number->string pos 16))
                                                   /dev/stdout)]))
                            (display #\] /dev/stdout))
                          (display fname /dev/stdout)
                          (display ": " /dev/stdout)
                          (unless (equal? datum dorig)
                            (display #\space /dev/stdout)
                            (write-datum dorig /dev/stdout)
                            (display " => " /dev/stdout))
                          (stdio-write-field datum width radix write-datum /dev/stdout)
                          (when (and (exact-nonnegative-integer? datum) (not (= radix 10)))
                            (display " (" /dev/stdout)
                            (write-datum datum /dev/stdout)
                            (display #\) /dev/stdout))
                          (when (> doffs 0)
                            (display " [+" /dev/stdout)
                            (write-datum doffs /dev/stdout)
                            (display #\] /dev/stdout))
                          (newline /dev/stdout)
                          (flush-output /dev/stdout)))))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define try-read-signature : (-> Input-Port Bytes Boolean)
  (lambda [/dev/stdin signature]
    (define siglength : Index (bytes-length signature))
    
    (and (equal? signature (peek-nbytes /dev/stdin siglength))
         (read-bytes siglength /dev/stdin)
         #true)))

(define read-signature : (-> Input-Port Bytes Symbol Any * Void)
  (lambda [/dev/stdin signature who . errmsg]
    (unless (try-read-signature /dev/stdin signature)
      (apply throw-signature-error /dev/stdin who errmsg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-mn:bytes : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (read-nbytes /dev/stdin (read-msize /dev/stdin size))))

(define read-mn:bstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (bytes->string/utf-8 (read-mn:bytes /dev/stdin bsize) (default-stdin-error-char))))

(define read-mn:lcstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (read-nlcstring /dev/stdin (read-msize /dev/stdin bsize))))

(define read-ln:bytes : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (read-nbytes /dev/stdin (read-lsize /dev/stdin size))))

(define read-ln:bstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (bytes->string/utf-8 (read-ln:bytes /dev/stdin bsize) (default-stdin-error-char))))

(define read-ln:lcstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (read-nlcstring /dev/stdin (read-lsize /dev/stdin bsize))))

(define-read-integer*
  [msb-bytes->octet 1 [read-msint8  #:-> Fixnum]  [read-muint8  #:-> Byte]]
  [msb-bytes->short 2 [read-msint16 #:-> Fixnum]  [read-muint16 #:-> Index]]
  [msb-bytes->int   4 [read-msint32 #:-> Fixnum]  [read-muint32 #:-> Index]]
  [msb-bytes->long  8 [read-msint64 #:-> Integer] [read-muint64 #:-> Natural]])

(define-read-integer*
  [lsb-bytes->octet 1 [read-lsint8  #:-> Fixnum]  [read-luint8  #:-> Byte]]
  [lsb-bytes->short 2 [read-lsint16 #:-> Fixnum]  [read-luint16 #:-> Index]]
  [lsb-bytes->int   4 [read-lsint32 #:-> Fixnum]  [read-luint32 #:-> Index]]
  [lsb-bytes->long  8 [read-lsint64 #:-> Integer] [read-luint64 #:-> Natural]])

(define-read-integer read-msize msb-bytes->index #:-> Index)
(define-read-integer read-lsize lsb-bytes->index #:-> Index)

(define-peek-integer*
  [msb-bytes->octet 1 [peek-msint8  #:-> Fixnum]  [peek-muint8  #:-> Byte]]
  [msb-bytes->short 2 [peek-msint16 #:-> Fixnum]  [peek-muint16 #:-> Index]]
  [msb-bytes->int   4 [peek-msint32 #:-> Fixnum]  [peek-muint32 #:-> Index]]
  [msb-bytes->long  8 [peek-msint64 #:-> Integer] [peek-muint64 #:-> Natural]])

(define-peek-integer*
  [lsb-bytes->octet 1 [peek-lsint8  #:-> Fixnum]  [peek-luint8  #:-> Byte]]
  [lsb-bytes->short 2 [peek-lsint16 #:-> Fixnum]  [peek-luint16 #:-> Index]]
  [lsb-bytes->int   4 [peek-lsint32 #:-> Fixnum]  [peek-luint32 #:-> Index]]
  [lsb-bytes->long  8 [peek-lsint64 #:-> Integer] [peek-luint64 #:-> Natural]])

(define-peek-integer peek-msize msb-bytes->index #:-> Index)
(define-peek-integer peek-lsize lsb-bytes->index #:-> Index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-stdin-locale : (Parameterof (Option (U Symbol String))) (make-parameter 'utf-8))
(define default-stdin-error-char : (Parameterof (Option Char)) (make-parameter #\uFFFD))
(define default-stdout-locale : (Parameterof (Option (U Symbol String))) (make-parameter 'utf-8))
(define default-stdout-error-byte : (Parameterof (Option Byte)) (make-parameter #false))
(define default-stdout-all-fields? : (Parameterof Boolean) (make-parameter #true))

(define string-locale-length : (-> String Index)
  (lambda [raw]
    (unicode-string-locale-length raw (default-stdout-locale))))

(define read-bytes* : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (cond [(bytes? bs) bs]
          [(raise (throw-eof-error /dev/stdin 'read-bytes*))])))

(define read-nbytes : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin 'read-nbytes)])))

(define read-nbstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (bytes->string/utf-8 (read-nbytes /dev/stdin bsize)
                         (default-stdin-error-char) 0 bsize)))

(define read-nlcstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin bsize]
    (define raw : Bytes (read-nbytes /dev/stdin bsize))
    (define lc-all : (U String Symbol False) (default-stdin-locale))

    (locale-bytes->unicode-string #:error-char (default-stdin-error-char)
                                  raw lc-all 0 bsize)))

(define read-tail-string : (-> Input-Port Integer (U String Char False) String)
  (lambda [/dev/stdin tailsize ?leader]
    (define-values (start total fill-char)
      (cond [(char? ?leader) (values 1 (+ tailsize 1) ?leader)]
            [(not ?leader) (values 0 tailsize #\null)]
            [else (let ([headsize (string-length ?leader)])
                    (values headsize (+ tailsize headsize) #\null))]))

    (define whole-string : String (make-string total fill-char))

    (when (string? ?leader)
      (string-copy! whole-string 0 ?leader 0))

    (read-string! whole-string /dev/stdin start total)
    whole-string))

(define peek-bytes* : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (if (bytes? bs) bs (throw-eof-error /dev/stdin 'peek-bytes*))))

(define peek-nbytes : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin 'peek-nbytes)])))

(define peek-nbstring : (-> Input-Port Natural String)
  (lambda [/dev/stdin size]
    (bytes->string/utf-8 (peek-nbytes /dev/stdin size)
                         (default-stdin-error-char) 0 size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-unlimited-octadecimal : (->* (Input-Port) (Natural #:\s?$? Boolean #:skip Natural) Natural)
  (lambda [/dev/stdin [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (read-unlimited-decimal /dev/stdin 3 char-octdigit? char->octadecimal skip result eat-last-whitespace?)))
 
(define read-unlimited-hexadecimal : (->* (Input-Port) (Natural #:\s?$? Boolean #:skip Natural) Natural)
  (lambda [/dev/stdin [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (read-unlimited-decimal /dev/stdin 4 char-hexdigit? char->hexadecimal skip result eat-last-whitespace?)))

(define-limited-unicode-reader read-limited-octadecimal #:->* (Input-Port Byte) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Nonnegative-Fixnum
  #:lambda [/dev/stdin max-ndigit [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (read-limited-decimal /dev/stdin max-ndigit 3 char-octdigit? char->octadecimal skip result eat-last-whitespace?))

(define-limited-unicode-reader read-limited-hexadecimal #:->* (Input-Port Byte) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Nonnegative-Fixnum
  #:lambda [/dev/stdin max-ndigit [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (read-limited-decimal /dev/stdin max-ndigit 4 char-hexdigit? char->hexadecimal skip result eat-last-whitespace?))

(define peek-flexible-octadecimal : (->* (Input-Port Natural) (Nonnegative-Fixnum Nonnegative-Fixnum #:ceiling Nonnegative-Fixnum) (Values Nonnegative-Fixnum Byte))
  (lambda [/dev/stdin skip [result 0] [count 0] #:ceiling [ceiling #o777]]
    (peek-flexible-decimal /dev/stdin skip 3 ceiling char-octdigit? char->octadecimal result count)))

(define peek-flexible-hexadecimal : (->* (Input-Port Natural) (Nonnegative-Fixnum Nonnegative-Fixnum #:ceiling Nonnegative-Fixnum) (Values Nonnegative-Fixnum Byte))
  (lambda [/dev/stdin skip [result 0] [count 0] #:ceiling [ceiling (assert #xFFFFFFFF fixnum?)]]
    (peek-flexible-decimal /dev/stdin skip 4 ceiling char-hexdigit? char->hexadecimal result count)))

(define read-unlimited-unicode-from-octadecimal : (->* (Input-Port) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Char)
  (lambda [/dev/stdin [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (integer->char (read-unlimited-octadecimal /dev/stdin result #:s?$? eat-last-whitespace? #:skip skip))))

(define-limited-unicode-reader read-limited-unicode-from-octadecimal #:->* (Input-Port Byte) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Char
  #:lambda [/dev/stdin max-ndigit [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]] #:-> integer->char
    (read-limited-octadecimal /dev/stdin max-ndigit result #:s?$? eat-last-whitespace? #:skip skip))

(define read-unlimited-unicode-from-hexadecimal : (->* (Input-Port) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Char)
  (lambda [/dev/stdin [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]]
    (integer->char (read-unlimited-hexadecimal /dev/stdin result #:s?$? eat-last-whitespace? #:skip skip))))

(define-limited-unicode-reader read-limited-unicode-from-hexadecimal #:->* (Input-Port Byte) (Nonnegative-Fixnum #:\s?$? Boolean #:skip Natural) Char
  #:lambda [/dev/stdin max-ndigit [result 0] #:\s?$? [eat-last-whitespace? #false] #:skip [skip 0]] #:-> integer->char
    (read-limited-hexadecimal /dev/stdin max-ndigit result #:s?$? eat-last-whitespace? #:skip skip))

(define peek-unicode-from-octadecimal : (->* (Input-Port Natural) (Nonnegative-Fixnum Nonnegative-Fixnum #:ceiling Nonnegative-Fixnum) (Values Char Byte))
  (lambda [/dev/stdin skip [result 0] [count 0] #:ceiling [ceiling #o777]]
    (define-values (n size) (peek-flexible-octadecimal /dev/stdin skip result count #:ceiling ceiling))

    (values (integer->char n) size)))

(define peek-unicode-from-hexadecimal : (->* (Input-Port Natural) (Nonnegative-Fixnum Nonnegative-Fixnum #:ceiling Nonnegative-Fixnum) (Values Char Byte))
  (lambda [/dev/stdin skip [result 0] [count 0] #:ceiling [ceiling (assert #xFFFFFFFF fixnum?)]]
    (define-values (n size) (peek-flexible-hexadecimal /dev/stdin skip result count #:ceiling ceiling))

    (values (integer->char n) size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define drop-string : (-> Input-Port Integer Void)
  (let* ([pool-size 4096]
         [/dev/null (make-string pool-size)])
    (lambda [/dev/stdin n]
      (let drop ([n : Integer n])
        (when (> n 0)
          (define count : (U Natural EOF)
            (read-string! /dev/null /dev/stdin 0 (min n pool-size)))
          (unless (eof-object? count)
            (drop (- n count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-write-integer* write-fixed-integer #true
  [1 [write-msint8]  [write-muint8]]
  [2 [write-msint16] [write-muint16]]
  [4 [write-msint32] [write-muint32]]
  [8 [write-msint64] [write-muint64]])

(define-write-integer* write-fixed-integer #false
  [1 [write-lsint8]  [write-luint8]]
  [2 [write-lsint16] [write-luint16]]
  [4 [write-lsint32] [write-luint32]]
  [8 [write-lsint64] [write-luint64]])

(define write-mn:bytes : (->* (Bytes Natural) (Output-Port) Nonnegative-Fixnum)
  (lambda [bs nsize [/dev/stdout (current-output-port)]]
    (define bsize : Index (bytes-length bs))
    
    (+ (write-muintptr bsize nsize /dev/stdout)
       (write-nbytes bs bsize /dev/stdout))))

(define write-mn:bstring : (->* (String Natural) (Output-Port) Nonnegative-Fixnum)
  (lambda [s nsize [/dev/stdout (current-output-port)]]
    (define bsize : Index (string-utf-8-length s))
    
    (+ (write-muintptr bsize nsize /dev/stdout)
       (write-nbstring s 0 /dev/stdout))))

(define write-ln:bstring : (->* (String Natural) (Output-Port) Nonnegative-Fixnum)
  (lambda [s nsize [/dev/stdout (current-output-port)]]
    (define bsize : Index (string-utf-8-length s))
    
    (+ (write-luintptr bsize nsize /dev/stdout)
       (write-nbstring s 0 /dev/stdout))))

(define write-mn:lcstring : (->* (String Natural) (Output-Port) Nonnegative-Fixnum)
  (lambda [s nsize [/dev/stdout (current-output-port)]]
    (define bs : Bytes (unicode-string->locale-bytes s (default-stdout-locale) #:error-byte (default-stdout-error-byte)))
    (define bsize : Index (bytes-length bs))
    
    (+ (write-muintptr bsize nsize /dev/stdout)
       (write-nbytes bs bsize /dev/stdout))))

(define write-ln:lcstring : (->* (String Natural) (Output-Port) Nonnegative-Fixnum)
  (lambda [s nsize [/dev/stdout (current-output-port)]]
    (define bs : Bytes (unicode-string->locale-bytes s (default-stdout-locale) #:error-byte (default-stdout-error-byte)))
    (define bsize : Index (bytes-length bs))
    
    (+ (write-luintptr bsize nsize /dev/stdout)
       (write-nbytes bs bsize /dev/stdout))))

(define write-msintptr : (->* (Integer Integer) (Output-Port) Byte)
  (lambda [n size [/dev/stdout (current-output-port)]]
    (write-fixed-integer /dev/stdout n size #true #true)))

(define write-lsintptr : (->* (Integer Integer) (Output-Port) Byte)
  (lambda [n size [/dev/stdout (current-output-port)]]
    (write-fixed-integer /dev/stdout n size #true #false)))

(define write-muintptr : (->* (Integer Integer) (Output-Port) Byte)
  (lambda [n size [/dev/stdout (current-output-port)]]
    (write-fixed-integer /dev/stdout n size #false #true)))

(define write-luintptr : (->* (Integer Integer) (Output-Port) Byte)
  (lambda [n size [/dev/stdout (current-output-port)]]
    (write-fixed-integer /dev/stdout n size #false #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-nbytes : (->* (Bytes Natural) (Output-Port) Index)
  (lambda [bs size [/dev/stdout (current-output-port)]]
    (cond [(= size 0) (write-bytes bs /dev/stdout 0 (bytes-length bs))]
          [else (write-bytes bs /dev/stdout 0 size)])))

(define write-nbstring : (->* (String Natural) (Output-Port) Index)
  (lambda [s bsize [/dev/stdout (current-output-port)]]
    (cond [(> bsize 0) (write-bytes (string->bytes/utf-8 s (default-stdout-error-byte)) /dev/stdout 0 bsize)]
          [else (let ([ch-size (write-string s /dev/stdout)])
                  (string-utf-8-length s 0 ch-size))])))

(define write-nlcstring : (->* (String Natural) (Output-Port) Index)
  (lambda [s bsize [/dev/stdout (current-output-port)]]
    (define lc-all : (U String Symbol False) (default-stdout-locale))
    (define bs : Bytes (unicode-string->locale-bytes s lc-all #:error-byte (default-stdout-error-byte)))
    
    (cond [(> bsize 0) (write-bytes bs /dev/stdout 0 bsize)]
          [else (write-bytes bs /dev/stdout)])))

(define pad-nlcstring : (->* (String Natural) (Output-Port) Index)
  (lambda [s bsize [/dev/stdout (current-output-port)]]
    (define lc-all : (U String Symbol False) (default-stdout-locale))
    (define bs : Bytes (unicode-string->locale-bytes s lc-all #:error-byte (default-stdout-error-byte)))
    (define dsize : Integer (- bsize (bytes-length bs)))
    
    (cond [(<= dsize 0) (write-bytes bs /dev/stdout 0 bsize)]
          [else (write-bytes (bytes-append bs (make-bytes dsize 0)) /dev/stdout)])))
