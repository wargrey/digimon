#lang racket

(provide main)

(require make)
(require raco/command-name)

(require compiler/cm)
(require compiler/compiler)

(require setup/setup)
(require setup/option)
(require setup/dirs)

(require "c/dyn.rkt")

(require "../digitama/system.rkt")
(require "../digitama/collection.rkt")
(require "../echo.rkt")
(require "../format.rkt")
(require "../debug.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-make-real-targets (make-parameter null))
(define current-make-phony-goal (make-parameter #false))

(define make-dry-run (make-parameter #false))
(define make-always-run (make-parameter #false))
(define make-just-touch (make-parameter #false))
(define make-trace-log (make-parameter #false))
(define make-errno (make-parameter 1))

(define make-restore-options!
  (lambda []
    (parallel-workers (processor-count))
    (compiler-verbose #true)
    
    (make-print-dep-no-line #false)
    (make-print-checking #false)
    (make-print-reasons #false)

    (make-trace-log #false)
    (make-dry-run #false)
    (make-always-run #false)
    (make-just-touch #false)
    (make-errno 1)))

(define make-set-verbose!
  (lambda []
    (for-each (λ [make-verbose] (make-verbose #true))
              (list make-print-dep-no-line make-print-checking
                    make-print-reasons make-trace-log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hack-rule
  (lambda [r]
    (define t (car r))
    (define ds (cadr r))
    (define f (thunk (let* ([t-already-exists? (file-exists? t)]
                            [tmp (make-temporary-file (~a (file-name-from-path t) ".~a") (and t-already-exists? t))])
                       (dynamic-wind (thunk (make-parent-directory* t))
                                     (thunk ((caddr r) t))
                                     (thunk (when (make-dry-run)
                                              (cond [t-already-exists? (rename-file-or-directory tmp t #true)]
                                                    [(file-exists? t) #| now exists |# (delete-file t)])))))))
    (list t (if (make-always-run) (cons (current-directory) ds) ds)
          (cond [(false? (make-just-touch)) f]
                [else (thunk (file-or-directory-modify-seconds t (current-seconds) f))]))))

(define do-compile
  (lambda [pwd digimons info-ref]
    (if (and (pair? digimons) (> (parallel-workers) 1))
        (compile-collection digimons)
        (compile-directory pwd info-ref))))

(define compile-collection
  (lambda [digimons [round 1]]
    (define summary? #false)
    (define (colorize-stdout bstr)
      (when (= round 1)
        (when (regexp-match? #px"--- summary of errors ---"  bstr) (set! summary? #true))
        (term-colorize 248 #false null (bytes->string/utf-8 bstr))))
    (define (colorize-stderr bstr)
      (term-colorize (if summary? 'red 224) #false null (bytes->string/utf-8 bstr)))
    (set!-values (again? compiling-round) (values #false round))
    (parameterize ([setup-program-name (short-program+command-name)]
                   [make-launchers #false]
                   [make-info-domain #false]
                   [make-foreign-libs #false]
                   [call-install #false]
                   [call-post-install #false]
                   [current-output-port (filter-write-output-port (current-output-port) colorize-stdout)]
                   [current-error-port (filter-write-output-port (current-error-port) colorize-stderr)])
      (or (setup #:collections (list digimons) #:make-docs? #false #:fail-fast? #true)
          (error 'wisemon "compiling failed.")))
    (when again? (compile-collection digimons (add1 round)))))

(define compile-directory
  (lambda [pwd info-ref [round 1]]
    (define px.in (pregexp (path->string (current-directory))))
    (define traceln (curry printf "round[~a]: ~a~n" round))
    (set! again? #false)
    (define (filter-verbose info)
      (match info
        [(pregexp #px"checking:") (when (and (make-print-checking) (regexp-match? px.in info)) (traceln info))]
        [(pregexp #px"compiling ") (set! again? #true)]
        [(pregexp #px"done:") (when (regexp-match? px.in info) (traceln info) (set! again? #true))]
        [(pregexp #px"maybe-compile-zo starting") (traceln info)]
        [(pregexp #px"(wrote|compiled|processing:|maybe-compile-zo finished)") '|Skip Task Endline|]
        [(pregexp #px"(newer|skipping:)") (when (make-print-reasons) (traceln info))]
        [_ (traceln info)]))
    (with-handlers ([exn? (λ [e] (error 'make "[error] ~a" (exn-message e)))])
      (parameterize ([manager-trace-handler filter-verbose]
                     [error-display-handler (λ [s e] (eechof #:fgcolor 'red ">> ~a~n" s))])
        (compile-directory-zos pwd info-ref #:verbose #false #:skip-doc-sources? #true)))
    (when again? (compile-directory pwd info-ref (add1 round)))))

(define smart-dependencies
  (lambda [entry [memory null]]
    (foldl (λ [subpath memory]
             (define subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
             (cond [(member subsrc memory) memory]
                   [else (smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file* entry
             (curry regexp-match* #px"(?<=@(include-section|require)[{[](\\(submod \")?).+?.(scrbl|rktl?)(?=[\"}])")))))

(define make-implicit-dist-rules
  (lambda [info-ref]
    (define digimon-tamer (build-path (current-directory) "tamer"))
    (define handbooks (find-digimon-handbooks info-ref))
    (cond [(or (null? handbooks) (string=? digimon-partner "root")) null]
          [else (for/list ([dependent.scrbl (in-value (car handbooks))])
                  (define t (build-path (current-directory) "README.md"))
                  (define ds (filter file-exists? (list* "info.rkt" (smart-dependencies dependent.scrbl))))
                  (list t ds (λ [target]
                               (parameterize ([current-namespace (make-base-namespace)]
                                              [current-input-port /dev/eof] ; tell scribble this is rendering to markdown
                                              [exit-handler (thunk* (error 'make "[fatal] ~a needs a proper `exit-handler`!"
                                                                           (find-relative-path (current-directory) dependent.scrbl)))])
                                 (eval `(require (prefix-in markdown: scribble/markdown-render) scribble/core scribble/render))
                                 (eval `(render (let ([scribble:doc (dynamic-require ,dependent.scrbl 'doc)])
                                                  (list (struct-copy part scribble:doc [parts null])))
                                                (list ,(file-name-from-path target))
                                                #:dest-dir ,(path-only target) #:render-mixin markdown:render-mixin
                                                #:quiet? #false #:warn-undefined? #false))))))])))

(define make-native-library-rules
  (lambda [info-ref]
    (define (build-with-output-filter build/0)
      (define-values (/dev/ctool/stdin /dev/ctool/stdout) (make-pipe))
      (define rewriter (thread (thunk (for ([line (in-lines /dev/ctool/stdin)])
                                        (if (regexp-match? #px"^(xform-cpp|compile-extension|link-extension|change-runtime-path):" line)
                                            (echof #:fgcolor 'cyan "~a~n"
                                                   (regexp-replaces line (list (list #px"^xform-cpp:\\s+\\("         "cpp: ")
                                                                               (list #px"^compile-extension:\\s+\\(" "cc:  ")
                                                                               (list #px"^link-extension:\\s+\\("    "ld:  ")
                                                                               (list #px"^change-runtime-path:\\s+"  "dyn: ")
                                                                               (list (path->string (current-directory)) "./")
                                                                               (list #px"( -o .+?( |$))|(\\)$)" ""))))
                                            (eechof #:fgcolor 'yellow "~a~n" line))))))
      (dynamic-wind (thunk (void '(if build/0 runs in thread then make will not be stopped by the failure)))
                    (thunk (parameterize ([current-output-port /dev/ctool/stdout]
                                          [current-error-port /dev/ctool/stdout])
                               (build/0)))
                    (thunk (void (flush-output /dev/ctool/stdout)
                                 (close-output-port /dev/ctool/stdout)
                                 (thread-wait rewriter)))))
    (define stone-dir (path->string (digimon-path 'stone)))
    (define-values (cc ld) (values (c-compiler) (c-linker)))
    (define this-envs (environment-variables-copy (current-environment-variables)))
    (c-set-environment-variables! this-envs cc ld digimon-system)
    (foldl append null
           (for/list ([c (in-list (find-digimon-files (curry regexp-match? #px"\\.c$") (current-directory)))])
             (define contained-in-package? (string-prefix? (path->string c) stone-dir))
             (define tobj (c-object-destination c contained-in-package?))
             (define t (c-library-destination c contained-in-package?))
             (list (list tobj (include.h c)
                         (λ [target]
                           (build-with-output-filter
                            (thunk (let ([cflags (c-compiler-flags cc digimon-system)])
                                     (parameterize ([current-extension-compiler-flags (append (current-extension-compiler-flags) cflags)]
                                                    [current-extension-preprocess-flags (append (current-extension-preprocess-flags) cflags)]
                                                    [current-environment-variables this-envs])
                                       (compile-extension #false c target (c-include-paths cc digimon-system))))))))
                   (list t (list tobj)
                         (λ [target]
                           (build-with-output-filter
                            (thunk (let ([ldflags (c-linker-flags c ld digimon-system)])
                                     (parameterize ([current-standard-link-libraries null]
                                                    [current-extension-linker-flags (append (current-extension-linker-flags) ldflags)]
                                                    [current-environment-variables this-envs])
                                       (link-extension #false (list tobj) target))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~all:
  (lambda [digimons info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))

    (define (do-make rules0)
      (unless (null? rules0)
        (define-values (imts exts) (partition (curryr assoc rules0) (current-make-real-targets)))
        (let ([rules (map hack-rule rules0)])
          (make/proc rules (if (null? (current-make-real-targets)) (map car rules) imts)))
        (current-make-real-targets exts)))
    
    (do-make (make-native-library-rules info-ref))
    (do-compile (current-directory) digimons info-ref)

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake premake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        ;;; the next two lines should useless but who knows
        (do-make (make-native-library-rules info-ref))
        (do-compile (current-directory) digimons info-ref)))
    
    (do-make (make-implicit-dist-rules info-ref))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (do-make (foldr append null
                          (filter (λ [val] (with-handlers ([exn? (const #false)])
                                                  (andmap (λ [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                      (procedure-arity-includes? (third ?) 1))) val)))
                                  (filter-map (λ [var] (namespace-variable-value var #false (const #false)))
                                              (namespace-mapped-symbols))))))))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files make))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))
    
    (make/proc (list (list (current-directory) null (thunk '|I don't know how to make all these files|)))
               (current-make-real-targets))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake postmake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))))

(define make~clean:
  (lambda [digimons info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))

    (define (fclean dirty)
      (void (cond [(file-exists? dirty) (delete-file dirty)]
                  [(directory-exists? dirty) (delete-directory dirty)])
            (printf "make: deleted ~a~n" (simplify-path dirty))))

    (when (member (current-make-phony-goal) '["distclean" "maintainer-clean"])
      (for ([submake (in-list submakes)])
        (define clbpath `(submod ,submake make:files clobber))
        (when (module-declared? clbpath #true)
          (dynamic-require clbpath #false))))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (define px.filter (pregexp (string-join #:before-first "^(.+?:)?" #:after-last ":.+:"
                                                  (member (string-replace (current-make-phony-goal) #px"(?<!^)-?clean" "")
                                                          '["maintainer" "dist" "clean" "mostly"]) "|")))
          (for ([var (in-list (namespace-mapped-symbols))]
                #:when (regexp-match? px.filter (symbol->string var)))
            (for-each fclean (map (λ [val] (if (list? val) (car val) val))
                                  (namespace-variable-value var #false (thunk null))))))))
    
    (for-each fclean (map car (make-implicit-dist-rules info-ref)))
    (for-each fclean (reverse (find-digimon-files (curry regexp-match? (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))
                                                  (current-directory) #:search-compiled? #true)))))

(define make~check:
  (lambda [digimons info-ref]
    (let ([rules (map hack-rule (make-native-library-rules info-ref))])
      (unless (null? rules) (make/proc rules (map car rules))))
    (do-compile (current-directory) digimons info-ref)

    (for ([handbook (in-list (if (null? (current-make-real-targets)) (find-digimon-handbooks info-ref) (current-make-real-targets)))])
      (define ./handbook (find-relative-path (current-directory) handbook))
      (parameterize ([current-directory (path-only handbook)]
                     [current-namespace (make-base-namespace)])
        (if (regexp-match? #px"\\.rkt$" ./handbook)
            (parameterize ([exit-handler (λ [retcode] (when (and (integer? retcode) (<= 1 retcode 255))
                                                        (error 'make "[error] ~a breaks ~a!" ./handbook (~n_w retcode "testcase"))))])
              (define modpath `(submod ,handbook main))
              (when (module-declared? modpath #true)
                (dynamic-require `(submod ,handbook main) #false)))
            (parameterize ([exit-handler (thunk* (error 'make "[fatal] ~a needs a proper `exit-handler`!" ./handbook))])
              (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
              (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                             #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                             #:dest-dir ,(build-path (path-only handbook) (car (use-compiled-file-paths)))
                             #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))
                             #:quiet? #false #:warn-undefined? #false))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fphonies
  (parameterize ([current-namespace (variable-reference->namespace (#%variable-reference))])
    (let ([px~fmake: #px"^make~(.+?):$"])
      (for/hash ([var (in-list (namespace-mapped-symbols))]
                 #:when (namespace-variable-value var #false (thunk #false))
                 #:when (regexp-match? px~fmake: (symbol->string var)))
        (values (list-ref (regexp-match px~fmake: (symbol->string var)) 1)
                (namespace-variable-value var #false))))))

(define-values (flag-table --help --unknown)
  (values `[[usage-help ,(format "Carefully options are not exactly the same as those of GNU Make.~n")] ; make "~n" work
            [once-each [["-B" "--always-make"] ,(λ _ (make-always-run #true)) ["Unconditionally make all targets."]]
                       [["-i" "--ignore-errors"] ,(λ _ (make-errno 0)) ["Do not tell shell there are errors."]]
                       [["-n" "--dry-run"] ,(λ _ (make-dry-run #true)) ["Just make without updating targets. [Except *.rkt]"]]
                       [["-s" "--silent"] ,(λ _ (current-output-port /dev/null)) ["Just make and only display errors."]]
                       [["-t" "--touch"] ,(λ _ (make-just-touch #true)) ["Touch targets instead of remaking them if it exists."]]
                       [["-d" "--debug"] ,(λ _ (make-trace-log #true)) ["Print lots of debug information."]]
                       [["-v" "--verbose"] ,(λ _ (make-set-verbose!)) ["Build with verbose messages."]]
                       [["-j" "--jobs"] ,(λ [flag n] (parallel-workers (max (or (string->number n) (processor-count)) 1)))
                                        ["Use <n> parallel jobs." "n"]]]]
          (λ [-h] (foldl (λ [ph -h] (if (hash-has-key? fphonies (car ph)) (format "~a  ~a : ~a~n" -h (car ph) (cdr ph)) -h))
                         ((curry string-replace -h #px"  -- : .+?-h --'.")
                          ((curryr string-join (format "~n  ")
                                   #:before-first (format "~n where <phony-target> is one of~n  ")
                                   #:after-last (format "~n"))
                           '["all : Build the entire project without documentation. [default]"
                             "mostlyclean : Delete all except that can be however hard to be remade."
                             "clean : Delete all except that record the configuration."
                             "distclean : Delete all that are excluded in the distribution."
                             "maintainer-clean : Delete all that can be remade. [For Maintainers]"]))
                         (list (cons "install" "Install this software and documentation.")
                               (cons "uninstall" "Delete all the installed files and documentation.")
                               (cons "dist" "Create a distribution file of the source files.")
                               (cons "check" "Validate and generate test report along with documentation."))))
          (curry eechof #:fgcolor 'lightred "make: I don't know what does `~a` mean!~n")))

(define make-digimon
  (λ [infos reals phonies]
    (if (pair? infos)
        (for ([subinfo (in-list (cdr infos))])
          (make-digimon subinfo reals phonies))
        (let ([zone (pkg-info-zone infos)]
              [info-ref (pkg-info-ref infos)]
              [digimons (list (pkg-info-name infos))])
          (parameterize ([current-make-real-targets (map simple-form-path reals)]
                         [current-digimon (car digimons)]
                         [current-directory zone])
            (dynamic-wind (thunk (echof #:fgcolor 'green "Enter Digimon Zone: ~a~n" (current-digimon)))
                          (thunk (for/sum ([phony (in-list (if (null? phonies) (list "all") phonies))])
                                   (parameterize ([current-make-phony-goal phony])
                                     (with-handlers ([exn:break? (λ [e] 130)]
                                                     [exn? (λ [e] (eechof #:fgcolor 'red "~a~n" (string-trim (exn-message e))) (make-errno))])
                                       (file-or-directory-modify-seconds zone (current-seconds) void) ; Windows complains, no such directory
                                       (cond [(regexp-match? #px"clean$" phony) ((hash-ref fphonies "clean") digimons info-ref)]
                                             [(hash-ref fphonies phony (thunk #false)) => (λ [mk] (mk digimons info-ref))]
                                             [else (error 'make "I don't know how to make `~a`!" phony)]) 0))))
                          (thunk (echof #:fgcolor 'green "Leave Digimon Zone: ~a~n" (current-digimon)))))))))

(define main
  (lambda [argument-list]
    (make-restore-options!)
    (parse-command-line
     (short-program+command-name)
     argument-list
     flag-table
     (λ [!voids . targets]
       (dynamic-wind (thunk (thread trace-log))
                     (thunk (let ([digimons (collection-info)])
                              (cond [(not digimons) (eechof #:fgcolor 'red "fatal: not in a digimon zone.~n") (exit 1)]
                                    [else (let-values ([(reals phonies) (partition filename-extension targets)])
                                            (exit (time-apply* (thunk (make-digimon digimons reals phonies)))))])))
                     (thunk (log-message (current-logger) 'info 'make "Job Done!" eof))))
     '["phony-target|file-path"]
     (compose1 exit display --help)
     (compose1 exit (const 1) --unknown (curryr string-trim #px"[()]") (curry format "~a") values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: parameters are thread specific.
(define again? #false)
(define compiling-round 1)

(define trace-log
  (let ([/dev/log (make-log-receiver (current-logger) 'debug)])
    (lambda []
      (define log (sync/enable-break /dev/log))
      (cond [(not (vector? log)) (trace-log)]
            [(eof-object? (vector-ref log 2)) (make-restore-options!)]
            [(memq (vector-ref log 3) '(racket/contract optimizer place GC)) (trace-log)]
            [(eq? (vector-ref log 3) 'setup/parallel-build)
             (define pce (struct->vector (vector-ref log 2)))
             (define ce (struct->vector (vector-ref pce 2)))
             (unless (memq (vector-ref ce 3) '(locking already-done))
               (if (eq? (vector-ref ce 3) 'finish-compile)
                   (echof #:fgcolor 250 "round[~a]: processor[~a]: made: ~a~n" compiling-round (vector-ref pce 1) (vector-ref ce 2))
                   (printf "round[~a]: processor[~a]: making: ~a~n" compiling-round (vector-ref pce 1) (vector-ref ce 2))))
             (set! again? #true)
             (trace-log)]
            [(make-trace-log)
             (match log  
               [(vector 'debug message _ _) (echof #:fgcolor 248 "~a~n" message)]
               [(vector 'info message _ _) (echof #:fgcolor 'cyan "~a~n" message)]
               [(vector 'warning message _ _) (echof #:fgcolor 'yellow "~a~n" message)]
               [(vector (or 'error 'fatal) message _ _) (echof #:fgcolor 'red "~a~n" message)]
               [_ (void)])
             (trace-log)]
            [else (trace-log)]))))

(define find-digimon-files
  (lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude
      (let ([cmpls (remove-duplicates (map (λ [p] (path->string (file-name-from-path p))) (use-compiled-file-paths)))])
        (pregexp (if search-compiled? "/\\.git$" (string-join #:before-first "/(\\.git|" #:after-last ")$" cmpls "|")))))
    (filter predicate (sequence->list (in-directory start-path (curry (negate regexp-match?) px.exclude))))))

(define find-digimon-handbooks
  (lambda [info-ref]
    (define maybe-handbooks (info-ref 'scribblings (thunk null)))
    (cond [(not (list? maybe-handbooks)) (raise-user-error 'info.rkt "malformed `scribblings`: ~a" maybe-handbooks)]
          [else (filter file-exists?
                        (for/list ([handbook (in-list maybe-handbooks)])
                          (cond [(pair? handbook) (build-path (current-directory) (car handbook))]
                                [else (raise-user-error 'info.rkt "malformed `scribblings`: ~a" handbook)])))])))

(define filter-write-output-port
  (lambda [/dev/stdout write-wrap [close? #false]]
    (make-output-port (object-name /dev/stdout)
                      /dev/stdout
                      (λ [bytes start end flush? enable-break?]
                        (define transformed (write-wrap (subbytes bytes start end)))
                        (cond [(string? transformed) (write-string transformed /dev/stdout)]
                              [(bytes? transformed) (write-bytes-avail* transformed /dev/stdout)])
                        (- end start))
                      (λ [] (unless (not close?) (close-output-port /dev/stdout)))
                      #false #false #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
