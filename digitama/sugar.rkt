#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require racket/path)
(require racket/list)

(require (for-syntax racket/base))
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
      (if (not rmp) '<nota-module> (resolved-module-path-name rmp))))

(define-syntax (#%file stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path (Pairof Path (Listof Symbol))))])
      (cond [(path? full) full]
            [(pair? full) (car full)]
            [else (current-directory)])))

(define-syntax (#%module stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path (Pairof Path (Listof Symbol))))])
      (cond [(path? full) (string->symbol (path->string (path-replace-extension (assert (file-name-from-path full) path?) "")))]
            [(pair? full) (last (cdr full))]
            [else '<anonymous>])))

(define-syntax (#%line stx)
  #`(quote #,(syntax-line stx)))

(define-syntax (#%column stx)
  #`(quote #,(syntax-column stx)))

(define-syntax (#%position stx)
  #`(quote #,(syntax-position stx)))

(define-syntax (#%location stx)
  #`(cons #,(syntax-line stx)
          #,(syntax-column stx)))

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

(define-syntax (define-type/enum stx)
  (syntax-case stx [: quote]
    [(_ id : TypeU (quote enum0) (quote enum) ...)
     #'(define-type/enum id : TypeU enum0 enum ...)]
    [(_ id : TypeU [enum0 comments0 ...] [enum comments ...] ...)
     #'(define-type/enum id : TypeU enum0 enum ...)]
    [(_ id : TypeU enum0 enum ...)
     (with-syntax ([id? (format-id #'id "belong-to-~a?" (syntax-e #'id))]
                   [id?* (format-id #'id "contained-in-~a?" (syntax-e #'id))]
                   [TypeU* (format-id #'TypeU "~a*" (syntax-e #'TypeU))])
     #'(begin (define-type TypeU (U 'enum0 'enum ...))
              (define-type TypeU* (Listof TypeU))
              (define id : (Pairof TypeU TypeU*) (cons 'enum0 (list 'enum ...)))
              (define-predicate id? TypeU)
              (define id?* : (-> (Listof Any) Boolean : #:+ TypeU*)
                (λ [es] (andmap (λ [e] (id? e)) es)))))]))

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
