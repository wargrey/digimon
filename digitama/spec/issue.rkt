#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/list)
(require racket/symbol)

(require "../../emoji.rkt")
(require "../../format.rkt")
(require "../../echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Issue-Type (U 'misbehaved 'todo 'skip 'panic 'pass))
(define-type Spec-Sexps (Listof Syntax))
(define-type Spec-Issue-Format-Datum (U String Void False))
(define-type Spec-Issue-Format (-> Any (-> Any Spec-Issue-Format-Datum) Spec-Issue-Format-Datum))

(define default-spec-issue-brief : (Parameterof (Option String)) (make-parameter #false))

(define default-spec-issue-expectation : (Parameterof (Option Symbol)) (make-parameter #false))
(define default-spec-issue-message : (Parameterof (Option String)) (make-parameter #false))
(define default-spec-issue-location : (Parameterof (Option srcloc)) (make-parameter #false))
(define default-spec-issue-expressions : (Parameterof (Syntaxof Spec-Sexps)) (make-parameter #'(list)))
(define default-spec-issue-arguments : (Parameterof (Listof Symbol)) (make-parameter null))
(define default-spec-issue-parameters : (Parameterof (Listof Any)) (make-parameter null))
(define default-spec-issue-exception : (Parameterof (Option exn:fail)) (make-parameter #false))
(define default-spec-issue-format : (Parameterof (Option Spec-Issue-Format)) (make-parameter #false))

(define default-spec-issue-rootdir : (Parameterof Path) current-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-issue
  ([type : Spec-Issue-Type]
   [brief : (Option String)]

   [expectation : (Option Symbol)]
   [message : (Option String)]
   [location : (Option srcloc)]
   [expressions : (Syntaxof Spec-Sexps)]
   [arguments : (Listof Symbol)]
   [parameters : (Listof Any)]
   [exception : (Option exn:fail)]
   
   [format : (Option Spec-Issue-Format)])
  #:type-name Spec-Issue
  #;transparent)

(define make-spec-issue : (-> Spec-Issue-Type Spec-Issue)
  (lambda [type]
    (spec-issue type
                (default-spec-issue-brief)
                
                (default-spec-issue-expectation)
                (default-spec-issue-message)
                (default-spec-issue-location)
                (default-spec-issue-expressions)
                (default-spec-issue-arguments)
                (default-spec-issue-parameters)
                (default-spec-issue-exception)

                (default-spec-issue-format))))

;; NOTE
; If a panic issue is catched, it means the code of the specification itself has bugs.
; Or `expect-throw` and `expect-no-exception` should be used instead.
(define make-spec-panic-issue : (-> exn:fail Spec-Issue)
  (lambda [e]
    (spec-issue (if (exn:fail:unsupported? e) 'skip 'panic)
                (default-spec-issue-brief)
                
                (default-spec-issue-expectation)
                (default-spec-issue-message)
                (default-spec-issue-location)
                (default-spec-issue-expressions)
                (default-spec-issue-arguments)
                (default-spec-issue-parameters)
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

(define spec-issue-moji : (-> Spec-Issue-Type (U Char String))
  (lambda [type]
    (case type
      [(pass) green-heart#]
      [(misbehaved) broken-heart#]
      [(skip) arrow-heart#]
      [(todo) growing-heart#]
      [(panic) bomb#])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-issue-misbehavior-display : (->* (Spec-Issue) (Symbol #:indent String) Void)
  (lambda [issue [color 'darkred] #:indent [headspace ""]]
    (let ([message (spec-issue-message issue)])
      (unless (not message)
        (spec-display-message headspace color null 'reason message)))
    
    (let ([location (spec-issue-location issue)])
      (unless (not location)
        (eechof #:fgcolor color "~a location: ~a~n" headspace
                (parameterize ([current-directory-for-user (default-spec-issue-rootdir)])
                  (srcloc->string location)))))

    (let ([e (spec-issue-exception issue)])
      (unless (not e)
        (spec-display-message headspace color null (object-name e) (exn-message e))))

    (let ([exprs (syntax-e (spec-issue-expressions issue))]
          [argus (map symbol->immutable-string (spec-issue-arguments issue))]
          [paras (map (spec-make-param->string (spec-issue-format issue)) (spec-issue-parameters issue))])
      (when (spec-issue-expectation issue)
        (eechof #:fgcolor color "~a expectation: ~a~n" headspace (spec-issue-expectation issue)))
      
      (when (pair? argus)
        (define asize : Index (apply max (map string-length argus)))
        (define psize : Index (apply max (map string-length paras)))

        (for ([expr (in-list exprs)]
              [argu (in-list argus)]
              [para (in-list paras)])
          (eechof #:fgcolor color "~a   ~a~a: ~a" headspace (~space (max (- asize (string-length argu)) 0)) argu para)
          (eechof #:fgcolor 'darkgrey " ~a; ~s~n" (~space (max (- psize (string-length para)) 0)) (syntax->datum expr)))))))
  
(define spec-issue-error-display : (->* (Spec-Issue) (Symbol #:indent String) Void)
  (lambda [issue [color 'darkred] #:indent [headspace ""]]
    (define errobj : (Option exn:fail) (spec-issue-exception issue))

    (unless (not errobj)
      (spec-display-message headspace color '(inverse) (object-name errobj) (exn-message errobj))
      (spec-display-stacks errobj 'darkgrey headspace))))

(define spec-issue-todo-display : (->* (Spec-Issue) (Symbol #:indent String) Void)
  (lambda [issue [color 'darkmagenta] #:indent [headspace ""]]
    (let ([location (spec-issue-location issue)])
      (unless (not location)
        (eechof #:fgcolor color "~a TODO: ~a~n" headspace
                (parameterize ([current-directory-for-user (default-spec-issue-rootdir)])
                  (srcloc->string location)))))
    
    (let ([message (spec-issue-message issue)])
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
        (spec-issue-misbehavior-display issue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-display-message : (-> String Symbol (Listof Symbol) Any String Void)
  (lambda [headspace color attributes prefix message]
    (define messages : (Listof String) (call-with-input-string message port->lines))
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
(define spec-make-param->string : (-> (Option Spec-Issue-Format) (-> Any String))
  (lambda [spec-format]
    (or (and spec-format
             (λ [para]
               (let* ([desc (spec-format para ~s)])
                 (if (string? desc) desc (~s para)))))
        ~s)))

(define spec-format-stack : (-> (Option Spec-Issue-Format) (Option Spec-Issue-Format) (Option Spec-Issue-Format))
  (lambda [usr-format fallback-format]
    (cond [(not usr-format) fallback-format]
          [(not fallback-format) usr-format]
          [else (λ [[para : Any] [fallback : (-> Any Spec-Issue-Format-Datum)]] : Spec-Issue-Format-Datum
                  (usr-format para
                              (λ [para]
                                (fallback-format para fallback))))])))
