#lang typed/racket/gui

(provide (all-defined-out))

(require racket/pretty)

(require "parameter.rkt")

(require "../wisemon/phony/cc.rkt")
(require "../wisemon/phony/typeset.rkt")
(require (only-in "../wisemon/parameter.rkt"
                  current-make-real-targets))

(require "../../wisemon.rkt")
(require "../../environ.rkt")
(require "../../dtrace.rkt")
(require "../../filesystem.rkt")
(require "../../spec.rkt")

(require "../../digitama/exec.rkt")
(require "../../digitama/system.rkt")
(require "../../digitama/collection.rkt")
(require "../../digitama/graphviz.rkt")
(require "../../digitama/git/lstree.rkt")
(require "../../digitama/git/langstat.rkt")

(require "../../digitama/toolchain/problem.rkt")
(require "../../digitama/toolchain/spec/clang.rkt")
(require "../../digitama/toolchain/cc/configuration.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-exec : (-> Path Any)
  (lambda [path]
    (dtrace-notice #:topic the-name "source: ~a" path)
    
    (define lang : (Option String)
      (or (wizarmon-lang)
          (let* ([gf (make-git-file path)]
                 [gl (git-files->langfiles (list gf) null git-default-subgroups)])
            (and (= (hash-count gl) 1)
                 (git-language-name (cdar (hash->list gl)))))))

    (when (or lang)
      (dtrace-notice #:topic the-name "language: ~a" lang))

    (case (and lang (string-downcase lang))
      [("racket") (shell-rkt path)]
      [("c++") (shell-cpp path 'cpp)]
      [("cpp") (shell-cpp path 'cpp)]
      [("c") (shell-c path 'c)]
      [("scribble") (shell-typeset path 'scribble)]
      [("tex") (shell-typeset path 'tex)]
      [("graphviz (dot)") (shell-dot path 'png #".png")]
      [else (wizarmon-errno 126)
            (raise (make-exn:fail:unsupported (if (not lang)
                                                  (format "exec: don't know how to run this file: ~a" path)
                                                  (format "exec: don't know how to run ~a file: ~a" lang path))
                                              (continuation-marks #false)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-rkt : (-> Path Any)
  (lambda [path.rkt]
    (parameterize ([current-namespace (make-gui-namespace)]
                   [global-port-print-handler pretty-print])
      (define main `(submod ,path.rkt main))
      
      (if (module-declared? main #true)
          (dynamic-require main 0)
          (dynamic-require path.rkt 0)))))

(define shell-c : (-> Path Symbol Natural)
  (lambda [path.c lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.c)
                                  (current-directory))))

    (parameterize ([current-make-real-targets (list path.c)]
                   [current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.c)))])
      (define-values (cc-specs.launchers targets) (make-cc-spec+targets (and maybe-info (pkg-info-ref maybe-info)) #false lang-name))

      (if (and cc-specs.launchers (pair? (cdr cc-specs.launchers)) (pair? targets))
          (let*-values ([(cc-specs launcher) (values (car cc-specs.launchers) (cdadr cc-specs.launchers))]
                        [(extra-libraries) (map path->complete-path (map path-normalize/system (c-path-flatten (cc-launcher-info-libpaths launcher))))])
            (wisemon-make cc-specs targets #:name the-name #:keep-going? #false)
            (define self-env : (Option Environment-Variables)
              (and (pair? extra-libraries)
                   (let ([env (environment-variables-copy (current-environment-variables))]
                         [epath (case (system-type 'os)
                                  [(macosx) #"DYLD_LIBRARY_PATH"]
                                  [(unix) #"LD_LIBRARY_PATH"]
                                  [else #"PATH"])])
                     (environment-variables-push-path! env #:name epath extra-libraries)
                     (dtrace-notice #:topic the-name "${~a}: ~a" epath (environment-variables-ref env epath))
                     env)))
            
            (parameterize ([current-environment-variables (or self-env (current-environment-variables))])
              (define problem-info : (Option Problem-Info) (read-clang-problem-info path.c))
              (define args : (Vectorof String) (current-command-line-arguments))
              (if (not problem-info)
                  (shell-exec-a.out (car targets) args #false)
                  (shell-exec-a.out problem-info (car targets) args 'debug))))
          127 #| deadcode, command cannot be found |#))))

(define shell-cpp : (-> Path Symbol Natural)
  (lambda [path.c lang-name]
    (shell-c path.c lang-name)))

(define shell-typeset : (-> Path Symbol Any)
  (lambda [path.scrbl lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.scrbl)
                                  (current-directory))))

    (parameterize ([current-make-real-targets (list path.scrbl)]
                   [current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.scrbl)))])
      (define all-typesettings : (Listof Tex-Info)
        (if (not maybe-info)
            (make-typeset-prepare "" #false)
            (make-typeset-prepare (pkg-info-name maybe-info)
                                  (pkg-info-ref maybe-info))))

      (when (pair? all-typesettings)
        (define-values (always-files ignored-files specs targets) (make-typeset-specs+targets all-typesettings))
      
        (make-typeset specs always-files ignored-files targets)
        (fg-recon-open-file 'exec (car targets))))))

(define shell-dot : (-> Path Symbol Bytes Any)
  (lambda [path.gv -T extension]
    (define dot.ext (assert (gv-script-destination path.gv extension #true)))

    (gv-render path.gv -T #:outfile dot.ext)
    (fg-recon-open-file 'exec dot.ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-echo-problem-info : (-> Problem-Info Void)
  (lambda [pinfo]
    (when (pair? (problem-info-description pinfo))
      (dtrace-debug "@~a:" 'problem)
      (for ([brief (in-list (problem-info-description pinfo))])
        (dtrace-note "~a" brief)))
    (when (pair? (problem-info-arguments pinfo))
      (dtrace-debug "@~a:" 'input)
      (for ([input (in-list (problem-info-arguments pinfo))])
        (dtrace-note "~a" input)))
    (when (problem-info-result pinfo)
      (dtrace-debug "@~a:" 'output)
      (when (problem-info-result pinfo)
        (dtrace-note "~a" (problem-info-result pinfo))))))

(define shell-exec-a.out : (case-> [Problem-Info Path (Vectorof String) (Option Dtrace-Level) -> Natural]
                                   [Path (Vectorof String) (Option Dtrace-Level) -> Natural])
  (case-lambda
    [(a.out args stdin-log-level)
     (define &status : (Boxof Nonnegative-Integer) (box 0))

     (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level stdin-log-level
                         #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                         'exec a.out args &status)
     (unbox &status)]
    [(problem-info a.out args stdin-log-level)
     (shell-echo-problem-info problem-info)
     (parameterize ([default-spec-exec-stdin-log-level stdin-log-level]
                    [default-spec-exec-stdout-port (current-output-port)])
       (spec-prove #:no-timing-info? #true #:no-location-info? #true #:no-argument-expression? #true
                   #:timeout (wizarmon-timeout) #:pre-spec dtrace-sync #:post-behavior dtrace-sync
                   (clang-problem->feature problem-info a.out args)))]))
