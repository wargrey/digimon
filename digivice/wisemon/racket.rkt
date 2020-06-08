#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/match)

(require typed/racket/unsafe)
(require typed/setup/getinfo)

(require "parameter.rkt")

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
 [setup (-> [#:collections (Option (Listof (Listof Path-String)))] [#:make-docs? Boolean] [#:fail-fast? Boolean] Boolean)])

(unsafe-require/typed
 compiler/cm
 [manager-trace-handler (Parameterof (-> String Any))])

(unsafe-require/typed
 compiler/compiler
 [compile-directory-zos (-> Path-String Info-Ref [#:verbose Boolean] [#:skip-doc-sources? Boolean] Void)])

(unsafe-require/typed
 racket/base
 [collection-file-path (All (a) (->* (Path-String #:fail (-> String a)) (#:check-compiled? Boolean) #:rest Path-String (U Path a)))])

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
    (define context : Symbol 'platform)
    (set!-values (again? compiling-round) (values #false round))

    (define (stdout-level [line : String]) : (Values Symbol (Option String))
      (values 'note
              (and (= round 1)
                   (case context
                     [(platform) (when (regexp-match? #px"main collects" line) (set! context 'paths))]
                     [(paths) (when (regexp-match? #px"---" line) (set! context 'compiling))]
                     [(compiling) (when (regexp-match? #px"--- summary of errors ---" line) (set! context 'summary))])
                   (or verbose! (not (eq? context 'paths)))
                   line)))

    (define (stderr-level [line : String]) : (Values Symbol (Option String))
      (values (if (eq? context 'summary) 'error 'warning) line))
    
    (parameterize ([setup-program-name (symbol->string the-name)]
                   [make-launchers #false]
                   [make-info-domain #false]
                   [make-foreign-libs #false]
                   [call-install #false]
                   [call-post-install #false]
                   [current-output-port (open-output-dtrace stdout-level)]
                   [current-error-port (open-output-dtrace stderr-level)])
      (or (setup #:collections (list (list digimon)) #:make-docs? #false #:fail-fast? #true)
          (error the-name "compiling failed.")))

    (when again? (compile-collection digimon (add1 round)))))

(define compile-directory : (->* (Path-String Info-Ref) (Natural) Void)
  (lambda [pwd info-ref [round 1]]
    (define verbose? : Boolean (make-verbose))
    (define px.in (pregexp (path->string (current-directory))))
    (define traceln (λ [[line : Any]] (dtrace-note "round[~a]: ~a" round line)))
    
    (set! again? #false)

    (define (filter-verbose [info : String])
      (match info
        [(pregexp #px"checking:") (when (and verbose? (regexp-match? px.in info)) (traceln info))]
        [(pregexp #px"compiling ") (set! again? #true)]
        [(pregexp #px"done:") (when (regexp-match? px.in info) (traceln info) (set! again? #true))]
        [(pregexp #px"maybe-compile-zo starting") (traceln info)]
        [(pregexp #px"(wrote|compiled|processing:|maybe-compile-zo finished)") '|Skip Task Endline|]
        [(pregexp #px"(newer|skipping:)") (when (and verbose?) (traceln info))]
        [_ (traceln info)]))

    (with-handlers ([exn:fail? (λ [[e : exn:fail]] (error the-name "[error] ~a" (exn-message e)))])
      (parameterize ([manager-trace-handler filter-verbose]
                     [error-display-handler (λ [s e] (dtrace-error ">> ~a" s))])
        (compile-directory-zos pwd info-ref #:verbose #false #:skip-doc-sources? #true)))

    (when again? (compile-directory pwd info-ref (add1 round)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define racket-smart-dependencies : (->* (Path-String) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (bytes->string/utf-8 subpath))))
             (cond [(member subsrc memory) memory]
                   [else (racket-smart-dependencies subsrc memory)]))
           (append memory (list (if (string? entry) (string->path entry) entry)))
           (call-with-input-file* entry
             (λ [[rktin : Input-Port]]
               (regexp-match* #px"(?<=@(include-(section|extracted|previously-extracted|abstract)|require)[{[]((\\(submod \")|\")?).+?.(scrbl|rktl?)(?=[\"}])"
                              rktin))))))

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
