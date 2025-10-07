#lang typed/racket/base

(provide (except-out (all-defined-out) Spec-Issue-Argument-Datum))

(require racket/list)
(require racket/symbol)
(require racket/match)
(require racket/format)

(require "../../emoji.rkt")
(require "../../echo.rkt")

(require "../minimal/string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Sexps (Listof (Syntaxof Any)))
(define-type Spec-Issue-Type (U 'misbehaved 'todo 'skip 'panic 'pass))
(define-type Spec-Issue-Message-Datum (U String (-> (Option String))))
(define-type Spec-Issue-Format-Datum (U String Void False))
(define-type Spec-Issue-Format (-> Any (-> Any Spec-Issue-Format-Datum) Spec-Issue-Format-Datum))
(define-type Spec-Issue-Argument-Datum (Immutable-Vector String (Pairof String (Listof String)) (Option (Syntaxof Any)) Byte))

(struct Spec-Syntax ([location : Syntax] [expressions : (Syntaxof Spec-Sexps)]) #:transparent)

(define default-spec-issue-brief : (Parameterof (Option String)) (make-parameter #false))

(define default-spec-issue-expectation : (Parameterof (Option Symbol)) (make-parameter #false))
(define default-spec-issue-message : (Parameterof (Option Spec-Issue-Message-Datum)) (make-parameter #false))
(define default-spec-issue-locations : (Parameterof (Listof Syntax)) (make-parameter null))
(define default-spec-issue-expressions : (Parameterof (Syntaxof Spec-Sexps)) (make-parameter #'(list)))
(define default-spec-issue-arguments : (Parameterof (Listof Any)) (make-parameter null))
(define default-spec-issue-parameters : (Parameterof (Listof Symbol)) (make-parameter null))
(define default-spec-issue-exception : (Parameterof (Option exn:fail)) (make-parameter #false))
(define default-spec-issue-format : (Parameterof (Option Spec-Issue-Format)) (make-parameter #false))
(define default-spec-issue-extra-arguments : (Parameterof (Listof Spec-Issue-Extra-Argument)) (make-parameter null))
(define default-spec-issue-ignored-arguments : (Parameterof (Listof Symbol)) (make-parameter null))

(define default-spec-issue-rootdir : (Parameterof Path) current-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-extra-argument
  ([name : Symbol]
   [value : Any]
   [syntax : (Option (Syntaxof Any))]
   [indent : Byte])
  #:type-name Spec-Issue-Extra-Argument
  #:transparent)

(define make-spec-extra-argument : (->* (Symbol (U Any (-> Any))) ((Syntaxof Any) #:indent Byte) Spec-Issue-Extra-Argument)
  (lambda [name val [syntax #false] #:indent [indent 0]]
    (spec-extra-argument name val syntax indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-issue
  ([type : Spec-Issue-Type]
   [brief : (Option String)]

   [expectation : (Option Symbol)]
   [message : (Option Spec-Issue-Message-Datum)]
   [locations : (Listof Syntax)]
   [expressions : (Syntaxof Spec-Sexps)]
   [arguments : (Listof Any)]
   [parameters : (Listof Symbol)]
   [extras : (Listof Spec-Issue-Extra-Argument)]
   [ignores : (Listof Symbol)]
   [exception : (Option exn:fail)]
   
   [format : (Option Spec-Issue-Format)])
  #:type-name Spec-Issue
  #:transparent)

(define make-spec-issue : (-> Spec-Issue-Type Spec-Issue)
  (lambda [type]
    (spec-issue type
                (default-spec-issue-brief)
                
                (default-spec-issue-expectation)
                (default-spec-issue-message)
                (default-spec-issue-locations)
                (default-spec-issue-expressions)
                (default-spec-issue-arguments)
                (default-spec-issue-parameters)
                (default-spec-issue-extra-arguments)
                (default-spec-issue-ignored-arguments)
                (default-spec-issue-exception)

                (default-spec-issue-format))))

;; NOTE
; If a panic issue is caught, it means the code of the specification itself has bugs.
; Or `expect-throw` and `expect-no-exception` should be used instead.
(define make-spec-panic-issue : (-> exn:fail Spec-Issue)
  (lambda [e]
    (spec-issue (if (exn:fail:unsupported? e) 'skip 'panic)
                (default-spec-issue-brief)
                
                (default-spec-issue-expectation)
                (default-spec-issue-message)
                (default-spec-issue-locations)
                (default-spec-issue-expressions)
                (default-spec-issue-arguments)
                (default-spec-issue-parameters)
                (default-spec-issue-extra-arguments)
                (default-spec-issue-ignored-arguments)
                e
                
                (default-spec-issue-format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-issue-fgcolor : (-> Spec-Issue-Type Symbol)
  (lambda [type]
    (case type
      [(pass) 'lightgreen]
      [(misbehaved) 'lightred]
      [(skip) 'lightblue]
      [(todo) 'lightmagenta]
      [(panic) 'darkred])))

(define spec-issue-emoji : (-> Spec-Issue-Type (U Char String))
  (lambda [type]
    (case type
      [(pass) green-heart#]
      [(misbehaved) broken-heart#]
      [(skip) arrow-heart#]
      [(todo) sandglass#]
      [(panic) bomb#])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-issue-misbehavior-display : (->* (Spec-Issue)
                                              (Symbol #:indent String #:no-location? Boolean #:no-argument-expression? Boolean)
                                              Void)
  (lambda [#:indent [headspace ""] #:no-location? [no-loc? #false] #:no-argument-expression? [no-exprs? #false]
           issue [color 'darkred]]
    (let ([message (spec-issue->message issue)])
      (unless (not message)
        (spec-display-message headspace color null 'reason message)))

    (when (not no-loc?)
      (spec-display-locations (spec-issue-locations issue) color headspace "location"))

    (let ([e (spec-issue-exception issue)])
      (unless (not e)
        (spec-display-message headspace color null (object-name e) (exn-message e))))

    (when (spec-issue-expectation issue)
      (eechof #:fgcolor color "~a expectation: ~a~n" headspace (spec-issue-expectation issue)))
    
    (define self-argv : (Listof Spec-Issue-Argument-Datum)
      (let ([f (spec-make-argument (spec-issue-format issue))]
            [ignores (spec-issue-ignores issue)])
        (for/list ([p (in-list (spec-issue-parameters issue))]
                   [a (in-list (spec-issue-arguments issue))]

                   ; WARNING: the issue expressions might also contain arguments
                   ;   that user-provided to create the issue message,
                   ;   despite the fact that `for/list` would ignore them.
                   [e (in-list (syntax-e (spec-issue-expressions issue)))]
                   #:unless (memq p ignores))
          (f p a e 0))))

    (define all-argv : (Listof Spec-Issue-Argument-Datum)
      (let ([es (spec-issue-extras issue)])
        (if (pair? es)
            (let ([eformat (spec-make-argument-for-extra (spec-issue-format issue))])
              (append self-argv
                      (for/list : (Listof Spec-Issue-Argument-Datum) ([e (in-list es)])
                        (eformat e))))
            self-argv)))

    (when (pair? all-argv)
      (define psize : Index (apply max (map spec-arg-name-length all-argv)))
      (define asize : Index (apply max (map spec-arg-head-length all-argv)))

      (for ([argu (in-list all-argv)])
        (define-values (name vals expr nested) (values (vector-ref argu 0) (vector-ref argu 1) (vector-ref argu 2) (vector-ref argu 3)))
        (define datum-space : String (~space (+ (- psize (string-length name)) nested)))
        (define subdatum-space : String (~space (+ psize nested)))
        (define expr-space : String (~space (- asize (string-length (car vals)))))
        
        (eechof #:fgcolor color "~a   ~a~a: ~a" headspace datum-space name (car vals))

        (if (and expr (not no-exprs?))
            (eechof #:fgcolor 'darkgrey " ~a; ~s~n" expr-space (syntax->datum expr))
            (eechof "~n"))
        
        (for ([subline (in-list (cdr vals))])
          (eechof #:fgcolor color "~a   ~a  ~a~n" headspace subdatum-space subline))))))

(define spec-issue-error-display : (->* (Spec-Issue) (Symbol #:indent String) Void)
  (lambda [issue [color 'darkred] #:indent [headspace ""]]
    (define errobj : (Option exn:fail) (spec-issue-exception issue))

    (unless (not errobj)
      (spec-display-message headspace color '(inverse) (object-name errobj) (exn-message errobj))

      (unless (exn:fail:user? errobj)
        (spec-display-stacks errobj 'darkgrey headspace)))))

(define spec-issue-todo-display : (->* (Spec-Issue) (Symbol #:indent String #:no-location? Boolean) Void)
  (lambda [issue [color 'darkmagenta] #:indent [headspace ""] #:no-location? [no-loc? #false]]
    (when (not no-loc?)
      (spec-display-locations (spec-issue-locations issue) color headspace "TODO"))
    
    (let ([message (spec-issue->message issue)])
      (unless (not message)
        (spec-display-message headspace color null 'reason message)))))

(define spec-issue-skip-display : (->* (Spec-Issue) (Symbol #:indent String) Void)
  (lambda [issue [color 'darkblue] #:indent [headspace ""]]
    (define errobj : (Option exn:fail) (spec-issue-exception issue))

    (unless (not errobj)
      (eechof #:fgcolor color "~a SKIP: ~a~n" headspace (object-name errobj))
      (spec-display-message headspace color null 'reason (exn-message errobj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-issue-display : (->* (Spec-Issue) (Output-Port #:indention Index) Void)
  (lambda [issue [/dev/stdout (current-output-port)] #:indention [indention 0]]
    (parameterize ([current-error-port /dev/stdout])
      (define type : Spec-Issue-Type (spec-issue-type issue))
      
      (fprintf /dev/stdout "~a - ~a~n" type (spec-issue-brief issue))
      
      (when (eq? type 'misbehaved)
        (spec-issue-misbehavior-display #:indent (if (> indention 0) (~space indention) "")
                                        issue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-display-locations : (-> (Listof Syntax) Symbol String String Void)
  (lambda [syntaxes color headspace tip]
    (define locations (reverse (filter-map spec-location syntaxes)))
    
    (when (pair? locations)
      (define subspace : String
        (cond [(null? (cdr locations)) headspace]
              [else (string-append headspace (make-string (+ (string-length tip) 2) #\space))]))
      
      (parameterize ([current-directory-for-user (default-spec-issue-rootdir)])
        (eechof #:fgcolor color "~a ~a: ~a~n" headspace tip (srcloc->string (car locations)))
        (for ([loc (in-list (cdr locations))])
          (eechof #:fgcolor color "~a ~a~n" subspace (srcloc->string loc)))))))

(define spec-display-message : (-> String Symbol (Listof Symbol) Any String Void)
  (lambda [headspace color attributes prefix message]
    (define messages : (Pairof String (Listof String)) (~string-lines message))
    (define head : String (format "~a: " prefix))

    (eechof #:fgcolor color #:attributes attributes "~a ~a~a~n" headspace head (car messages))

    (when (pair? (cdr messages))
      (define subspace (~space (string-length head)))
      
      (for ([submsg (in-list (cdr messages))])
        (eechof #:fgcolor color #:attributes attributes "~a ~a~a~n" headspace subspace submsg)))))

(define spec-display-stacks : (-> exn Symbol String Void)
  (lambda [errobj color headspace]
    (parameterize ([current-directory-for-user (default-spec-issue-rootdir)])
      (let display-stack ([stacks (continuation-mark-set->context (exn-continuation-marks errobj))]
                          [sofni : (Listof (Pairof Symbol String)) null])
        (if (pair? stacks)
            (let ([maybe-srcloc (cdar stacks)])
              (if (and (srcloc? maybe-srcloc) (srcloc-line maybe-srcloc) (srcloc-column maybe-srcloc))
                  (let ([fname (or (caar stacks) 'λ)]
                        [location (srcloc->string maybe-srcloc)])
                    (cond [(string? location) (display-stack (cdr stacks) (cons (cons fname location) sofni))]
                          [else (display-stack (cdr stacks) sofni)]))
                  (display-stack (cdr stacks) sofni)))
            (for ([info (in-list (reverse (remove-duplicates sofni)))])
              (eechof #:fgcolor color "~a »»»» ~a: ~a~n" headspace (cdr info) (car info))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-arg-name-length : (-> Spec-Issue-Argument-Datum Index)
  (lambda [ss]
    (if (zero? (vector-ref ss 3))
        (string-length (vector-ref ss 0))
        0)))

(define spec-arg-head-length : (-> Spec-Issue-Argument-Datum Index)
  (lambda [ss]
    (string-length (car (vector-ref ss 1)))))

(define spec-s : (-> Symbol Any (Option (Syntaxof Any)) Byte Spec-Issue-Argument-Datum)
  (lambda [name val expr indent]
    (vector-immutable (symbol->immutable-string name) (~string-lines (~s val)) expr indent)))

(define spec-make-argument : (-> (Option Spec-Issue-Format) (-> Symbol Any (Option (Syntaxof Any)) Byte Spec-Issue-Argument-Datum))
  (lambda [spec-format]
    (or (and spec-format
             (λ [[name : Symbol] [val : Any] [expr : (Option (Syntaxof Any))] [indent : Byte]]
               (let ([desc (spec-format val ~s)])
                 (vector-immutable (symbol->immutable-string name)
                                   (~string-lines (if (string? desc) desc (~s val)))
                                   expr
                                   indent))))
        spec-s)))

(define spec-make-argument-for-extra : (-> (Option Spec-Issue-Format) (-> Spec-Issue-Extra-Argument Spec-Issue-Argument-Datum))
  (lambda [spec-format]
    (define make-argv (spec-make-argument spec-format))
    
    (λ [[self : Spec-Issue-Extra-Argument]] : Spec-Issue-Argument-Datum
      (make-argv (spec-extra-argument-name self)
                 (spec-extra-argument-value self)
                 (spec-extra-argument-syntax self)
                 (spec-extra-argument-indent self)))))

(define spec-format-stack : (-> (Option Spec-Issue-Format) (Option Spec-Issue-Format) (Option Spec-Issue-Format))
  (lambda [usr-format fallback-format]
    (cond [(not usr-format) fallback-format]
          [(not fallback-format) usr-format]
          [else (λ [[para : Any] [fallback : (-> Any Spec-Issue-Format-Datum)]] : Spec-Issue-Format-Datum
                  (usr-format para
                              (λ [para]
                                (fallback-format para fallback))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-issue->message : (-> Spec-Issue (Option String))
  (lambda [self]
    (define message (spec-issue-message self))
    (and message
         (cond [(string? message) message]
               [else (message)]))))

(define spec-location : (-> Syntax (Option srcloc))
  (lambda [stx]
    (let ([src (syntax-source stx)]
          [line (syntax-line stx)]
          [column (syntax-column stx)])                        
      (and (or (path? src) (path-string? src)) line column
           (srcloc src line column (syntax-position stx) (syntax-span stx))))))
