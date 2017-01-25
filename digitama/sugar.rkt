#lang typed/racket

(provide (all-defined-out))

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
     #'(begin (provide (all-from-out spec)) ...
              (require spec) ...)]))

(define-syntax (require/provide/syntax stx)
  (syntax-case stx []
    [(_ spec ...)
     #'(begin (provide (for-syntax (all-from-out spec))) ...
              (require (for-syntax spec)) ...)]))

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     #'(require/typed/provide/batch modpath [id ...] type-definition)]
    [(_ modpath [id ...] type-definition)
     #'(require/typed/provide modpath [id type-definition] ...)]))

(define-syntax (#%full-module stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (if (false? rmp) '<nota-module> (resolved-module-path-name rmp))))

(define-syntax (#%file stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) full]
            [(pair? full) (car full)]
            [else (current-directory)])))

(define-syntax (#%module stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) (string->symbol (path->string (path-replace-extension (assert (file-name-from-path full) path?) "")))]
            [(pair? full) (last (cdr full))]
            [else '<anonymous>])))

(define-syntax (#%function stx) ; class method has a symbol name looks like "[name] method in [class%]"
  #'(let use-next-id : Symbol ([stacks (continuation-mark-set->context (current-continuation-marks))])
      (if (null? stacks) 'λ
          (or (caar stacks)
              (use-next-id (cdr stacks))))))

(define-syntax (throw stx)
  (syntax-parse stx
    [(_ st:id rest ...)
     #'(throw [st] rest ...)]
    [(_ [st:id argl ...] frmt:str v ...)
     #'(throw [st argl ...] (#%function) frmt v ...)]
    [(_ [st:id argl ...] src frmt:str v ...)
     #'(raise (st (format (string-append "~s: " frmt) src v ...) (current-continuation-marks) argl ...))]))

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
                       (cons (datum->syntax argument (string->keyword (symbol->string (car (syntax->datum argument)))))
                             (cons argument args)))])
       #'(begin (struct id ([property : DataType] ...) options ...)
                (define (make-instance args ...) : ID (id property-filter ...))
                (define-type ID id)))]))

(define-syntax (defconsts stx)
  (syntax-case stx [:]
    [(_ : Type [id val] ...)
     #'(begin (define id : Type val) ...)]))

(define-syntax (define-type/enum stx)
  (syntax-case stx [: quote]
    [(_ id : TypeU (quote enum) ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU [enum comments ...] ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU enum ...)
     (with-syntax ([id? (format-id #'id "belong-to-~a?" (syntax-e #'id))]
                   [id?* (format-id #'id "contained-in-~a?" (syntax-e #'id))]
                   [TypeU* (format-id #'TypeU "~a*" (syntax-e #'TypeU))])
     #'(begin (define-type TypeU (U 'enum ...))
              (define-type TypeU* (Listof TypeU))
              (define id : TypeU* (list 'enum ...))
              (define-predicate id? TypeU)
              (define id?* : (-> (Listof Any) Boolean : #:+ TypeU*)
                (λ [es] (andmap (λ [e] (id? e)) es)))))]))

;; prefab structures cannot be converted to contract in typed racket;
;; transparent structures are not allowed as place message.
(define-type UInt32 Nonnegative-Fixnum)  ; this is a bit smaller than uint32
(define-type UInt64 Nonnegative-Integer) ; this is larger than uint64
(define-type MPInteger Integer)
(define-type (nBytes n) Bytes)           ; the length is prefixed when n is String

(define-type Primitive-Type (Rec PT (U Symbol (List 'Listof PT) (List 'nBytes (U Natural 'String 'Bytes)))))

(define-syntax (define-type/consts stx)
  (syntax-case stx [: of as]
    [(_ cs : TypeU of Type (const val comments ...) ...)
     (with-syntax ([$%cs (format-id #'cs "$%~a" (syntax-e #'cs))]
                   [$#cs (format-id #'cs "$#~a" (syntax-e #'cs))])
       #'(begin (define-type TypeU (U 'const ...))
                (define $#cs : (-> TypeU Type)
                  (let ([cs : (HashTable TypeU Type) ((inst make-immutable-hasheq TypeU Type) (list (cons 'const val) ...))])
                    (lambda [sym] ((inst hash-ref TypeU Type Type) cs sym))))
                (define $%cs : (-> Type (Option TypeU))
                  (let ([cs : (HashTable Type TypeU) ((inst make-immutable-hasheq Type TypeU) (list (cons val 'const) ...))])
                    (lambda [v] ((inst hash-ref Type TypeU False) cs v (lambda [] #false)))))))]
    [(_ cs : TypeU of Type as parent (const val ([field : DataType] ...)) ...)
     (with-syntax* ([$*cs (format-id #'cs "$*~a" (syntax-e #'cs))]
                    [$:cs (format-id #'cs "$:~a" (syntax-e #'cs))]
                    [?parent (format-id #'parent "?~a" (syntax-e #'parent))])
       #'(begin (define-type/consts cs : TypeU of Type (const val) ...)
                (struct parent () #:prefab)
                (struct ?parent parent ([id : Type]) #:prefab)
                (struct const parent ([field : DataType] ...) #:prefab) ...
                (define $*cs : (-> (U TypeU Type) (Listof Any) parent)
                  ;;; use `val` instead of `const` does not work.
                  (λ [sym argl] (case sym [(const) (apply const (cast argl (List DataType ...)))] ... [else (?parent (cast sym Type))])))
                (define $:cs : (-> TypeU (Listof Primitive-Type))
                  (let ([cs : (HashTable TypeU (Listof Primitive-Type))
                         ((inst make-immutable-hasheq TypeU (Listof Primitive-Type))
                          (list (cons 'const (list 'DataType ...)) ...))])
                    (λ [sym] ((inst hash-ref TypeU (Listof Primitive-Type) (Listof Primitive-Type)) cs sym))))))]))

(define-syntax (match/handlers stx)
  (syntax-case stx [:]
    [(_ s-exp : type? match-clause ...)
     #'(match (with-handlers ([exn? (λ [[e : exn]] e)]) s-exp)
         match-clause ...
         [(? type? no-error-value) no-error-value])]
    [(_ s-exp match-clause ...)
     #'(match (with-handlers ([exn? (λ [[e : exn]] e)]) s-exp)
         match-clause ...
         [escaped-value escaped-value])]))
