#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require typed/racket/unsafe)
(require typed/setup/getinfo)

(require "parameter.rkt")
(require "cmdname.rkt")

(require "../../filesystem.rkt")
(require "../../dtrace.rkt")
(require "../../echo.rkt")

(unsafe-require/typed
 setup/option
 [setup-program-name (Parameterof String)]
 [make-launchers (Parameterof Boolean)]
 [make-info-domain (Parameterof Boolean)]
 [make-foreign-libs (Parameterof Boolean)]
 [call-install (Parameterof Boolean)]
 [call-post-install (Parameterof Boolean)])

(unsafe-require/typed
 setup/setup
 [setup (-> [#:collections (Option (Listof (Listof Path-String)))] [#:make-docs? Boolean] [#:fail-fast? Boolean] [#:recompile-only? Boolean] Boolean)])

(unsafe-require/typed
 compiler/cm
 [manager-trace-handler (Parameterof (-> String Any))])

(unsafe-require/typed
 compiler/compiler
 [compile-directory-zos (-> Path-String Info-Ref [#:verbose Boolean] [#:skip-doc-sources? Boolean] Void)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-compile : (-> Path-String String Info-Ref Void)
  (lambda [pwd digimon info-ref]
    (if (and ((inst collection-file-path Boolean) "." digimon #:fail (λ [[errmsg : String]] #false)) (> (parallel-workers) 1))
        (compile-collection digimon)
        (compile-directory pwd info-ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compile-collection : (->* (String) (Natural) Void)
  (lambda [digimon [round 1]]
    (define verbose! : Boolean (make-verbose))
    (define recompiling? : Boolean (> round 1))
    (define context : Symbol 'platform)
    (set!-values (again? compiling-round) (values #false round))

    (define (stdout-level [line : String]) : (Values Symbol (Option String))
      (values 'note
              (and (not recompiling?)
                   (case context
                     [(platform) (when (regexp-match? #px"main collects" line) (set! context 'paths))]
                     [(paths) (when (regexp-match? #px"---" line) (set! context 'compiling))]
                     [(compiling) (when (regexp-match? #px"--- summary of errors ---" line) (set! context 'summary))])
                   (or verbose! (not (eq? context 'paths)))
                   line)))

    (define (stderr-level [line : String]) : (Values Symbol (Option String))
      (values (if (eq? context 'summary) 'error 'warning) line))
    
    (parameterize ([setup-program-name (symbol->immutable-string (the-cmd-name))]
                   [make-launchers #false]
                   [make-info-domain #false]
                   [make-foreign-libs #false]
                   [call-install #false]
                   [call-post-install #false]
                   [current-output-port (open-output-dtrace stdout-level)]
                   [current-error-port (open-output-dtrace stderr-level)])
      (or (setup #:collections (list (list digimon)) #:make-docs? #false #:fail-fast? #true #:recompile-only? recompiling?)
          (error (string->symbol (setup-program-name)) "compiling failed.")))

    (when again? (compile-collection digimon (add1 round)))))

(define compile-directory : (->* (Path-String Info-Ref) (Natural #:for-typesetting? Boolean) Void)
  (lambda [pwd info-ref [round 1] #:for-typesetting? [for-typesetting? #false]]
    (define verbose? : Boolean (make-verbose))
    (define px.in : Regexp (pregexp (regexp-quote (path->string (current-directory)))))
    (define traceln (λ [[line : Any]] (dtrace-note "round[~a]: ~a" round line)))

    (set! again? #false)

    (define (filter-verbose [info : String])
      (cond [(regexp-match? #px"checking:" info) (when (and verbose? (regexp-match? px.in info)) (traceln info))]
            [(regexp-match? #px"compiling " info) (set! again? #true)]
            [(regexp-match? #px"done:" info) (when (regexp-match? px.in info) (traceln info) (set! again? #true))]
            [(regexp-match? #px"maybe-compile-zo starting" info) (traceln info)]
            [(regexp-match? #px"(wrote|compiled|processing:|maybe-compile-zo finished)" info) '|Skip Task Endline|]
            [(regexp-match? #px"(newer|skipping:)" info) (when (and verbose?) (traceln info))]
            [else (traceln info)]))

    (with-handlers ([exn:fail? (λ [[e : exn:fail]] (error (the-cmd-name) "[error] ~a" (exn-message e)))])
      (parameterize ([manager-trace-handler filter-verbose]
                     [error-display-handler (λ [s e] (dtrace-error ">> ~a" s))])
        (compile-directory-zos pwd info-ref #:verbose #false #:skip-doc-sources? (not for-typesetting?))))

    (when again? (compile-directory pwd info-ref (add1 round)))))

(define compile-scribble : (->* ((Listof Path)) (Path) Void)
  (lambda [targets [rootdir (current-directory)]]
    (define scrbls : (Listof Path)
      (for/list ([src (in-list targets)]
                 #:when (regexp-match? #px"[.]scrbl$" src))
        src))
    
    (define info-ref : Info-Ref
      (lambda [symid [fallback (λ [] (raise-user-error (the-cmd-name) "undefined symbol: `~a`" symid))]]
        (case symid
          [(scribblings) (if (pair? scrbls) (list scrbls) (fallback))]
          [else (fallback)])))

    (compile-directory rootdir info-ref #:for-typesetting? #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: deal with sources of Literate Programming suffixed with `.rkt`
(define at:require-kws : String "@(include-(section|extracted|previously-extracted|abstract)|require)")
(define required-filename : String "[^\\s]+?[.](scrbl|rktl?)")
(define nested-require-spec-behind : String "([(](\\w|-){1,16}\\s{0,4}){0,4}\"")
(define nested-require-spec-ahead : String "[^]}]*")

(define px.rkt-deps : PRegexp
  (pregexp (string-append "(?<=[(]require\\s{0,4}"
                          (string-append "(\"|" nested-require-spec-behind "))")
                          "[^\\s]+?[.]rktl?"
                          "(?=\"[^)]*[)]+)")))

(define px.scrbl-deps : PRegexp
  (pregexp (string-append #; lookbehind
                          (string-append "(?<="
                                         at:require-kws
                                         (string-append "([{]|"
                                                        "([[]" nested-require-spec-behind "))")
                                         ")")
                          
                          required-filename

                          #; lookahead
                          (string-append "(?=" "\"?"
                                         nested-require-spec-ahead
                                         "[]}]"
                                         ")"))))

(define racket-smart-dependencies : (->* (Path) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (path-identity subpath))))
             (cond [(member subsrc memory) memory]
                   [(regexp-match? #px"[.]scrbl$" subpath) (scribble-smart-dependencies subsrc memory)]
                   [else (racket-smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file* entry (λ [[rktin : Input-Port]] (regexp-match* px.rkt-deps rktin))))))

(define scribble-smart-dependencies : (->* (Path) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (path-identity subpath))))
             (cond [(member subsrc memory) memory]
                   [(regexp-match? #px"[.]rkt.?$" subpath) (racket-smart-dependencies subsrc memory)]
                   [else (scribble-smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file* entry (λ [[rktin : Input-Port]] (regexp-match* px.scrbl-deps rktin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: parameters are thread specific in which these variables case cannot be shared by `racket-trace-log`
(define again? : Boolean #false)
(define compiling-round : Natural 1)

(define racket-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (define info-color (if (make-dry-run) 'lightblue 'cyan))

    (cond [(make-dry-run)
           (case level
             [(info) (echof #:fgcolor info-color "~a~n" message)])]
          [(make-trace-log)
           (case level
             [(note) (echof #:fgcolor 246 "~a~n" message)]
             [(info) (echof #:fgcolor info-color "~a~n" message)]
             [else (dtrace-event-echo level message urgent topic)])]
          [(dtrace-level<? level 'note)
           (dtrace-event-echo level message urgent topic)])))

(define racket-setup-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (define pce (struct->vector urgent))
    (define ce (struct->vector (vector-ref pce 2)))
    (unless (memq (vector-ref ce 3) '(locking already-done))
      (if (eq? (vector-ref ce 3) 'finish-compile)
          (echof #:fgcolor 250 "round[~a]: processor[~a]: made: ~a~n" compiling-round (vector-ref pce 1) (vector-ref ce 2))
          (printf "round[~a]: processor[~a]: making: ~a~n" compiling-round (vector-ref pce 1) (vector-ref ce 2))))
    (set! again? #true)))

(define make-racket-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop #:topic-receivers (list (cons 'setup/parallel-build racket-setup-event-echo))
                      #:default-receiver racket-event-echo
                      (cond [(make-verbose) 'trace]
                            [(make-trace-log) 'note]
                            [else 'info]))))
