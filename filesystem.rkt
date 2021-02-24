#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/path racket/file))

(require racket/path)
(require racket/file)
(require racket/list)

(require typed/racket/date)

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

(define explode-path/strip : (-> (U Path-String Path-For-Some-System) Integer (Listof (U 'same 'up Path-For-Some-System)))
  (lambda [path strip]
    (define elements : (Listof (U 'same 'up Path-For-Some-System)) (explode-path path))
    
    (cond [(<= strip 0) elements]
          [(<= (length elements) strip) null]
          [else (drop elements strip)])))

(define explode-path/cleanse : (-> (U Path-String Path-For-Some-System) [#:strip Integer] (Listof (U 'same 'up Path-For-Some-System)))
  ; if 'same exists, it is the unique element, and the original path refers to current directory
  ; if 'up exists, it/they must appear at the beginning, and the original path refers to somewhere other than its subpaths. 
  (lambda [path #:strip [strict-count 0]]
    (cond [(> strict-count 0)
           (let ([es (explode-path/strip path strict-count)])
             (cond [(pair? es) (explode-path/cleanse (apply build-path es) #:strip 0)]
                   [else null]))]
          [else (explode-path (simplify-path path #false))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define path-exists? : (-> Path-String Boolean)
  (lambda [path]
    (or (link-exists? path)
        (file-exists? path)
        (directory-exists? path))))

(define path-add-sequence : (->* (Path-String) (String #:start Natural #:step Natural) (Option Path))
  (lambda [path [seqfmt ":~a"] #:start [seq0 2] #:step [step 1]]
    (define-values (parent basename syntactically-dir?) (split-path (simplify-path path #false)))
    (define .ext : (Option Bytes) (path-get-extension path))

    (and parent (path? basename)
         (let try-sequence : (Option Path) ([seq : Natural seq0])
           (define suffix : String (format seqfmt seq))
           (define pathname : String
             (if (or syntactically-dir? (not .ext))
                 (format "~a~a" basename suffix)
                 (format "~a~a~a" (path-replace-extension basename #"") suffix .ext)))
           (define fullname : Path
             (cond [(path? parent) (build-path parent pathname)]
                   [else (string->path pathname)]))

           (cond [(not (path-exists? fullname)) fullname]
                 [(not (= step 0)) (try-sequence (+ seq step))]
                 [else #false])))))

(define path-add-timestamp : (->* (Path-String) (Integer Boolean #:@ Any) (Option Path))
  (lambda [path [ts-seconds (current-seconds)] [local-time? #false] #:@ [@ #\@]]
    (define timestamp : String (date->string (seconds->date ts-seconds local-time?) #true))
    (define-values (parent basename syntactically-dir?) (split-path (simplify-path path #false)))
    (define .ext : (Option Bytes) (path-get-extension path))

    (define pathname : (Option String)
      (and (path? basename)
           (if (or syntactically-dir? (not .ext))
               (format "~a~a~a" basename @ timestamp)
               (format "~a~a~a~a" (path-replace-extension basename #"") @ timestamp .ext))))
    
    (and (string? pathname)
         (cond [(path? parent) (build-path parent pathname)]
               [(symbol? parent) (string->path pathname)]
               [else #false #| root directory should not be modified |#]))))

(define path-add-timestamp* : (->* (Path-String) (Integer Boolean #:@ Any) (Option Path))
  (lambda [path [ts-seconds (current-seconds)] [local-time? #false] #:@ [@ #\@]]
    (define newpath : (Option Path) (path-add-timestamp path ts-seconds local-time? #:@ @))

    (and newpath
         (cond [(not (path-exists? newpath)) newpath]
               [else (path-add-sequence newpath "[~a]")]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-path-match-predicates : (-> (U Regexp Byte-Regexp String Path (Listof (U Regexp Byte-Regexp String Path)))
                                         (Values (-> Path-String Boolean) (-> Path-String Boolean)))
  (lambda [matches]
    (define-values (px:matches eq:matches)
      (let partition : (Values (Listof (U Regexp Byte-Regexp)) (Listof Path))
        ([matches : (Listof (U Regexp Byte-Regexp String Path)) (if (list? matches) matches (list matches))]
         [sxp : (Listof (U Regexp Byte-Regexp)) null]
         [xqe : (Listof Path) null])
        (cond [(null? matches) (values (reverse sxp) (reverse xqe))]
              [else (let-values ([(self rest) (values (car matches) (cdr matches))])
                      (cond [(or (regexp? self) (byte-regexp? self)) (partition rest (cons self sxp) xqe)]
                            [(string? self) (partition rest sxp (cons (string->path self) xqe))]
                            [else (partition rest sxp (cons self xqe))]))])))
    
    (define (px:match? [fullpath : Path-String]) : Boolean
      (for/or ([px (in-list px:matches)])
        (regexp-match? px fullpath)))
    
    (define (eq:match? [basename : Path-String]) : Boolean
      (cond [(string? basename) (eq:match? (string->path basename))]
            [else (for/or ([eq (in-list eq:matches)])
                    (equal? eq basename))]))

    (values px:match? eq:match?)))

(define make-path-match-predicate : (-> (U Regexp Byte-Regexp String Path (Listof (U Regexp Byte-Regexp String Path)))
                                        (case-> [Path-String -> Boolean]
                                                [Path-String Path-String -> Boolean]))
  (lambda [matches]
    (define-values (px:match? eq:match?) (make-path-match-predicates matches))
    
    (case-lambda
      [([fullpath : Path-String])
       (or (px:match? fullpath)
           (let ([basename (file-name-from-path fullpath)])
             (and basename (eq:match? basename))))]
      [([fullpath : Path-String] [basename : Path-String])
       (or (px:match? fullpath)
           (eq:match? basename))])))
