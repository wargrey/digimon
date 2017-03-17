#lang racket

(provide main)

(require make)
(require raco/command-name)

(require compiler/cm)
(require compiler/xform)
(require compiler/compiler)

(require dynext/compile)
(require dynext/link)
(require dynext/file)

(require setup/setup)
(require setup/option)
(require setup/getinfo)
(require setup/dirs)

(require "../digitama/system.rkt")
(require "../echo.rkt")
(require "../format.rkt")

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
                    make-print-reasons make-trace-log verbose
                    make-verbose compiler-verbose))))

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

(define trace-log
  (let ([/dev/log (make-log-receiver (current-logger) 'debug)])
    (lambda []
      (define log (sync/enable-break /dev/log))
      (cond [(and (vector? log) (eof-object? (vector-ref log 2))) (make-restore-options!)]
            [else (when (make-trace-log)
                    (match log  
                      [(vector level message urgent 'racket/contract) (void)]
                      [(vector 'debug message _ _) (echof #:fgcolor 248 "racket: ~a~n" message)]
                      [(vector 'info message _ _) (echof #:fgcolor 'cyan "racket: ~a~n" message)]
                      [(vector 'warning message _ _) (echof #:fgcolor 'yellow "racket: ~a~n" message)]
                      [_ (void)]))
                  (trace-log)]))))

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

(define do-compile
  (lambda [pwd maybe-digimon info-ref]
    (if (and maybe-digimon (> (parallel-workers) 1))
        (compile-collection maybe-digimon)
        (compile-directory pwd info-ref))))

(define compile-collection
  (lambda [digimon]
    (parameterize ([setup-program-name (short-program+command-name)]
                   [make-launchers #false]
                   [make-docs #false]
                   [make-info-domain #false]
                   [make-foreign-libs #false]
                   [call-install #false]
                   [call-post-install #false])
      (setup #:collections (list (list digimon))))))

(define compile-directory
  (lambda [pwd info-ref [round 1]]
    (define again? (make-parameter #false))
    (define px.in (pregexp (path->string (current-directory))))
    (define traceln (curry printf "pass[~a]: ~a~n" round))
    (define (filter-verbose info)
      (match info
        [(pregexp #px"checking:") (when (and (make-print-checking) (regexp-match? px.in info)) (traceln info))]
        [(pregexp #px"compiling ") (again? #true)]
        [(pregexp #px"done:") (when (regexp-match? px.in info) (traceln (string-replace info "done:" "processed")) (again? #true))]
        [(pregexp #px"maybe-compile-zo starting") (traceln (string-replace info "maybe-compile-zo starting" "compiling"))]
        [(pregexp #px"(wrote|compiled|processing:|maybe-compile-zo finished)") '|Skip Task Endline|]
        [(pregexp #px"(newer|skipping:)") (when (make-print-reasons) (traceln info))]
        [_ (traceln info)]))
    (with-handlers ([exn? (λ [e] (error 'make "[error] ~a" (exn-message e)))])
      (parameterize ([manager-trace-handler filter-verbose]
                     [error-display-handler (lambda [s e] (eechof #:fgcolor 'red ">> ~a~n" s))])
        (compile-directory-zos pwd info-ref #:verbose #false #:skip-doc-sources? #true)))
    (when (again?) (compile-directory pwd info-ref (add1 round)))))

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
    (define (include.h entry racket? [memory null])
      (foldl (lambda [include memory]
               (cond [(regexp-match #px#"<.+?>" include)
                      => (lambda [header]
                           (when (member #"<scheme.h>" header) (racket? #true))
                           memory)]
                     [(regexp-match #px#"\"(.+?)\"" include)
                      => (lambda [header]
                           (let ([subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 (cadr header))))])
                             (cond [(member subsrc memory) memory]
                                   [else (include.h subsrc racket? memory)])))]))
             (append memory (list entry))
             (call-with-input-file* entry (curry regexp-match* #px"(?<=#include )[<\"].+?.h[\">]"))))
    (define (dynamic-ldflags c)
      (for/fold ([ldflags (list* "-m64" "-shared"
                                 (cond [(false? (symbol=? (digimon-system) 'macosx)) null]
                                       [else (list "-L/usr/local/lib" (~a "-F" (find-lib-dir)) "-framework" "Racket")]))])
                ([line (in-list (file->lines c))]
                 #:when (regexp-match? #px"#include <" line))
        (define modeline (regexp-match #px".+ld:(\\w+)?:?([^*]+)(\\*/)?$" line))
        (cond [(false? modeline) ldflags #| In the future it will support "-L" and `pkg-config` |#]
              [else (match-let ([(list _ hint ld _) modeline])
                      (for/fold ([ld-++ ldflags])
                                ([flags (in-port read (open-input-string ld))]
                                 #:when (pair? flags) #| filter out empty list |#)
                        (match (cons (digimon-system) (and hint (map string->symbol (string-split hint ":"))))
                          [(list 'macosx 'framework) ; /* ld:framework: (IOKit) */
                           (append ld-++ (let ([-fw (list (~a #\- hint))])
                                           (add-between (map ~a flags) -fw #:splice? #true #:before-first -fw)))]
                          [(cons _ (or (? false?) (? (curry memq (digimon-system))))) ; /* ld: (ssh2) or ld:illumos: (kstat) */
                           (append ld-++ (map (curry ~a "-l") flags))]
                          [_ ldflags])))])))
    (define (build-with-output-filter build/0)
      (define-values (/dev/ctool/stdin /dev/ctool/stdout) (make-pipe))
      (define rewriter (thread (thunk (for ([line (in-lines /dev/ctool/stdin)])
                                        (if (regexp-match? #px"^(xform-cpp|compile-extension|link-extension|change-runtime-path):" line)
                                            (echof #:fgcolor 'cyan "~a~n"
                                                   (regexp-replaces line (list (list #px"^xform-cpp:\\s+\\("         "cpp: ")
                                                                               (list #px"^compile-extension:\\s+\\(" "cc:  ")
                                                                               (list #px"^link-extension:\\s+\\("    "ld:  ")
                                                                               (list #px"^change-runtime-path:\\s+"  "dyn: ")
                                                                               (list (path->string (current-directory)) ".")
                                                                               (list #px"( -o .+?( |$))|(\\)$)" ""))))
                                            (eechof #:fgcolor 'yellow "~a~n" line))))))
      (dynamic-wind (thunk (void '(if build/0 runs in thread then make will not be stopped by the failure)))
                    (thunk (parameterize ([current-output-port /dev/ctool/stdout]
                                          [current-error-port /dev/ctool/stdout])
                               (build/0)))
                    (thunk (void (flush-output /dev/ctool/stdout)
                                 (close-output-port /dev/ctool/stdout)
                                 (thread-wait rewriter)))))
    (foldl append null
           (for/list ([c (in-list (find-digimon-files (curry regexp-match? #px"\\.c$") (current-directory)))])
             (define racket? (make-parameter #false))
             (define-values [tobj t]
               (values (build-path (path-only c) (car (use-compiled-file-paths))
                                   "native" (system-library-subpath #false)
                                   (append-object-suffix (extract-base-filename/c (file-name-from-path c))))
                       (build-path (path-only c) (car (use-compiled-file-paths))
                                   "native" (system-library-subpath #false)
                                   (path-replace-extension (file-name-from-path c) (system-type 'so-suffix)))))
             (list (list tobj (include.h c racket?)
                         (lambda [target]
                           (build-with-output-filter
                            (thunk (let ([cflags (list (format "-std=~a11" (if (racket?) "gnu" "c")) "-m64"
                                                       (format "-D__~a__" (digimon-system)))])
                                     (parameterize ([current-extension-compiler-flags (append cflags (current-extension-compiler-flags))]
                                                    [current-extension-preprocess-flags
                                                     (append cflags (current-extension-preprocess-flags))])
                                       ; meanwhile `xform` is buggy
                                       ;(define xform.c (box (build-path (path-only target) (file-name-from-path c))))
                                       (define -Is (list (current-directory) "/usr/local/include"))
                                       ;(cond [(false? (racket?)) (set-box! xform.c c)]
                                       ;      [else (xform #false c (unbox xform.c) -Is #:keep-lines? #true)]) ; gcc knows file lines
                                       (compile-extension #false c target -Is)))))))
                   (list t (list tobj)
                         (lambda [target]
                           (build-with-output-filter
                            (thunk (let ([ldflags (dynamic-ldflags c)])
                                     (parameterize ([current-standard-link-libraries null]
                                                    [current-extension-linker-flags ldflags])
                                       (link-extension #false (list tobj) target)
                                       (case (system-type 'os)
                                         [(macosx) (let ([image (format "Racket.framework/Versions/~a_~a/Racket"
                                                                        (version) (system-type 'gc))])
                                                     (define change-path (format "~a -change ~a ~a ~a"
                                                                                 (find-executable-path "install_name_tool")
                                                                                 image (format "~a/~a" (find-lib-dir) image) t))
                                                     (printf "change-runtime-path: ~a~n" change-path) ; this will be redirected
                                                     (system change-path))]))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~all:
  (lambda [maybe-digimon info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))

    (define (do-make rules0)
      (unless (null? rules0)
        (define-values (imts exts) (partition (curryr assoc rules0) (current-make-real-targets)))
        (let ([rules (map hack-rule rules0)])
          (make/proc rules (if (null? (current-make-real-targets)) (map car rules) imts)))
        (current-make-real-targets exts)))
    
    (do-make (make-native-library-rules info-ref))
    (do-compile (current-directory) maybe-digimon info-ref)

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake premake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        ;;; the next two lines should useless but who knows
        (do-make (make-native-library-rules info-ref))
        (do-compile (current-directory) maybe-digimon info-ref)))
    
    (do-make (make-implicit-dist-rules info-ref))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (do-make (foldr append null
                          (filter (lambda [val] (with-handlers ([exn? (const #false)])
                                                  (andmap (lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                           (procedure-arity-includes? (third ?) 1))) val)))
                                  (filter-map (lambda [var] (namespace-variable-value var #false (const #false)))
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
  (lambda [maybe-digimon info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))

    (define (fclean dirty)
      (void (cond [(file-exists? dirty) (delete-file dirty)]
                  [(directory-exists? dirty) (delete-directory dirty)])
            (printf "make: deleted ~a.~n" (simplify-path dirty))))

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
            (for-each fclean (map (lambda [val] (if (list? val) (car val) val))
                                  (namespace-variable-value var #false (thunk null))))))))
    
    (for-each fclean (map car (make-implicit-dist-rules info-ref)))
    (for-each fclean (reverse (find-digimon-files (curry regexp-match? (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))
                                                  (current-directory) #:search-compiled? #true)))))

(define make~check:
  (lambda [maybe-digimon info-ref]
    (let ([rules (map hack-rule (make-native-library-rules info-ref))])
      (unless (null? rules) (make/proc rules (map car rules))))
    (do-compile (current-directory) maybe-digimon info-ref)

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
                             #:render-mixin (lambda [%] (html:render-multi-mixin (html:render-mixin %)))
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
  (λ [zone targets]
    (define-values (reals phonies) (partition filename-extension targets))
    (define info-ref (get-info/full zone))
    (define digimon
      (or (and info-ref (let ([name (info-ref 'collection)]) (and (string? name) name)))
          (let-values ([(base pkg-name dir?) (split-path zone)]) pkg-name)))
    (define maybe-digimon (with-handlers ([exn? (λ _ #false)]) (and (collection-file-path "info.rkt" digimon) digimon)))
    (parameterize ([current-make-real-targets (map simple-form-path reals)]
                   [current-directory zone])
      (dynamic-wind (thunk (echof #:fgcolor 'green "Enter Digimon Zone: ~a~n" digimon))
                    (thunk (for/sum ([phony (in-list (if (null? phonies) (list "all") phonies))])
                             (parameterize ([current-make-phony-goal phony])
                               (with-handlers ([exn? (λ [e] (eechof #:fgcolor 'red "~a~n" (string-trim (exn-message e))) (make-errno))])
                                 (file-or-directory-modify-seconds zone (current-seconds))
                                 (cond [(regexp-match? #px"clean$" phony) ((hash-ref fphonies "clean") maybe-digimon info-ref)]
                                       [(hash-ref fphonies phony (thunk #false)) => (λ [mk] (mk maybe-digimon info-ref))]
                                       [else (error 'make "I don't know how to make `~a`!" phony)]) 0))))
                    (thunk (echof #:fgcolor 'green "Leave Digimon Zone: ~a~n" digimon))))))

(define main
  (lambda [argument-list]
    (make-restore-options!)
    (parse-command-line
     (short-program+command-name)
     argument-list
     flag-table
     (λ [!voids . targets]
       (dynamic-wind (thunk (thread trace-log))
                     (thunk (let cd ([pwd (current-directory)])
                              (cond [(get-info/full pwd) (exit (make-digimon pwd targets))]
                                    [else (let-values ([(base name dir?) (split-path pwd)])
                                            (cond [(false? base) (eechof #:fgcolor 'red "fatal: not in a digimon zone.~n") (exit 1)]
                                                  [else (cd base)]))])))
                     (thunk (log-message (current-logger) 'info 'make "Job Done!" eof))))
     '["phony-target|file-path"]
     (compose1 exit display --help)
     (compose1 exit (const 1) --unknown (curryr string-trim #px"[()]") (curry format "~a") values))))

(main (current-command-line-arguments))
