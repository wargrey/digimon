#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/path racket/file))

(require racket/path)
(require racket/file)
(require racket/list)

(require "port.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-file-reader stx)
  (syntax-parse stx #:datum-literals [lambda λ]
    [(_ id #:+ Type
        (~or (~and #:binary binary-flag)
             (~and #:text text-flag))
        (~or #:lambda #:λ) do-read)
     (quasisyntax/loc stx
       (define id : (-> Path-String [#:mode (U 'binary 'text)] [#:count-lines? Boolean] Type)
         (let ([up-to-dates : (HashTable Path (Pairof Type Nonnegative-Fixnum)) (make-hash)])
           (lambda [#:mode [mode 'binary]
                    #:count-lines? [count-lines? #,(if (attribute text-flag) #'(port-count-lines-enabled) #'#false)]
                    src]
             (define mtime : Nonnegative-Fixnum (file-or-directory-modify-seconds src))
             (define file.src : Path (simplify-path src))
             (define mdatum : (Option (Pairof Type Nonnegative-Fixnum)) (hash-ref up-to-dates file.src (λ [] #false)))
             (cond [(and mdatum (<= mtime (cdr mdatum))) (car mdatum)]
                   [else (let ([datum (parameterize ([port-count-lines-enabled count-lines?])
                                        (call-with-input-file* file.src #:mode mode
                                          (λ [[/dev/stdin : Input-Port]] : Type
                                            (do-read /dev/stdin file.src))))])
                           (hash-set! up-to-dates file.src (cons datum mtime))
                           datum)])))))]
    [(_ id #:+ Type mode:keyword ((~or lambda λ) [/dev/stdin src] body ...))
     (with-syntax ([id* (format-id #'id "~a*" (syntax-e #'id))])
       (syntax/loc stx
         (begin (define id : (case-> [Input-Port Path -> Type]
                                     [Input-Port -> Type])
                  (case-lambda
                    [(/dev/stdin) (id /dev/stdin (port-path /dev/stdin))]
                    [(/dev/stdin src) body ...]))
                
                (define-file-reader id* #:+ Type mode #:lambda id))))]
    [(_ id #:+ Type (~or #:lambda #:λ) do-read) (syntax/loc stx (define-file-reader id #:+ Type #:binary #:lambda do-read))]
    [(_ id #:+ Type (do-read ...)) (syntax/loc stx (define-file-reader id #:+ Type #:binary (do-read ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dirname : (-> Path-String [#:rootname String] String)
  (lambda [path #:rootname [root "/"]]
    (define dir : (Option Path-String)
      (let ([dir : Path (simple-form-path path)])
        (cond [(directory-exists? path) path]
              [else (path-only path)])))
    (cond [(not dir) #| DEADCODE |# root]
          [else (let-values ([(_b name _?) (split-path dir)])
                  (cond [(path? name) (path->string name)]
                        [else root]))])))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))

(define file-executable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'execute (file-or-directory-permissions p))
         #true)))

(define file-mtime : (->* (Path-String) (Nonnegative-Fixnum) Nonnegative-Fixnum)
  (lambda [f [fallback 0]]
    (cond [(file-exists? f) (file-or-directory-modify-seconds f)]
          [else fallback])))

(define file-touch : (All (a) (case-> [Path-String (-> a) -> (U Void a)]
                                      [Path-String -> Void]))
  (case-lambda
    [(target on-touch-error)
     (file-or-directory-modify-seconds target (assert (current-seconds) exact-nonnegative-integer?) on-touch-error)]
    [(target)
     (file-or-directory-modify-seconds target (assert (current-seconds) exact-nonnegative-integer?)
                                       (λ [] (unless (file-exists? target)
                                               (make-parent-directory* target)
                                               (call-with-output-file* target void))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-root-relative-path : (-> (U Path-String Path-For-Some-System) Path-For-Some-System)
  (lambda [path]
    (cond [(relative-path? path) (if (string? path) (string->path path) path)]
          [else (let ([elements (explode-path path)])
                  (cond [(or (null? elements) (null? (cdr elements))) (build-path 'same)]
                        [else (apply build-path (cdr elements))]))])))

(define explode-path/strict : (-> (U Path-String Path-For-Some-System) Integer (Listof (U 'same 'up Path-For-Some-System)))
  (lambda [path strip]
    (define elements : (Listof (U 'same 'up Path-For-Some-System)) (explode-path path))
    
    (cond [(<= strip 0) elements]
          [(<= (length elements) strip) null]
          [else (drop elements strip)])))

(define explode-path/clean : (-> (U Path-String Path-For-Some-System) [#:strip Integer] (Listof (U 'same 'up Path-For-Some-System)))
  ; if 'same exists, it is the unique element, and the original path refers to current directory
  ; if 'up exists, it/they must appear at the beginning, and the original path refers to somewhere other than its subpaths. 
  (lambda [path #:strip [strict-count 0]]
    (cond [(> strict-count 0)
           (let ([es (explode-path/strict path strict-count)])
             (cond [(pair? es) (explode-path/clean (apply build-path es) #:strip 0)]
                   [else null]))]
          [else (explode-path (simplify-path path #false))])))
