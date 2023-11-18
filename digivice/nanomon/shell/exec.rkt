#lang typed/racket/gui

(provide (all-defined-out))

(require racket/pretty)

(require "../shell.rkt")
(require "../parameter.rkt")

(require "../../wisemon/phony/cc.rkt")
(require (only-in "../../wisemon/parameter.rkt" current-make-real-targets))

(require "../../../wisemon.rkt")
(require "../../../environ.rkt")
(require "../../../dtrace.rkt")

(require "../../../digitama/exec.rkt")
(require "../../../digitama/collection.rkt")
(require "../../../digitama/git/lstree.rkt")
(require "../../../digitama/git/langstat.rkt")

(require "../../../digitama/toolchain/cc/configuration.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell~~exec : (-> Path Thread Any)
  (lambda [path env-thread]
    (dtrace-notice #:topic the-name "target: ~a" path)
    
    (define lang : (Option String)
      (or (nanomon-lang)
          (let* ([gf (make-git-file path)]
                 [gl (git-files->langfiles (list gf) null #false)])
            (and (= (hash-count gl) 1)
                 (git-language-name (cdar (hash->list gl)))))))

    (when (or lang)
      (dtrace-notice #:topic the-name "language: ~a" lang))
    
    (case (and lang (string-downcase lang))
      [("racket") (shell-rkt path)]
      [("c++") (shell-cpp path 'cpp)]
      [("cpp") (shell-cpp path 'cpp)]
      [("c") (shell-c path 'c)]
      [else (nanomon-errno 126)
            (raise (make-exn:fail:unsupported (format "exec: don't know how to run this script: ~a" path)
                                              (continuation-marks #false)))])
    
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-rkt : (-> Path Any)
  (lambda [path.rkt]
    (parameterize ([current-namespace (make-gui-namespace)]
                   [global-port-print-handler pretty-print])
      (define main `(submod ,path.rkt main))
      
      (if (module-declared? main #true)
          (dynamic-require main 0)
          (dynamic-require path.rkt 0)))))

(define shell-c : (-> Path Symbol Any)
  (lambda [path.c lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.c)
                                  (current-directory))))

    (parameterize ([current-make-real-targets (list path.c)]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.c)))])
      (define-values (cc-specs.launchers targets) (make-cc-spec+targets (and maybe-info (pkg-info-ref maybe-info)) #false lang-name))

      (if (and cc-specs.launchers (pair? (cdr cc-specs.launchers)) (pair? targets))
          (let*-values ([(cc-specs launcher) (values (car cc-specs.launchers) (cdadr cc-specs.launchers))]
                        [(extra-libraries) (c-path-flatten (cc-launcher-info-libpaths launcher))])
            (wisemon-make cc-specs targets #:name the-name #:keep-going? #false)
            (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level #false
                                #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                                #:env (and (pair? extra-libraries)
                                           (let ([env (environment-variables-copy (current-environment-variables))]
                                                 [epath (case (system-type 'os)
                                                          [(macosx) #"DYLD_LIBRARY_PATH"]
                                                          [(unix) #"LD_LIBRARY_PATH"]
                                                          [else #"PATH"])])
                                             (environment-variables-push-path! env #:name epath extra-libraries)
                                             (dtrace-notice #:topic the-name "${~a}: ~a" epath (environment-variables-ref env epath))
                                             env))
                           'exec (car targets) (list (vector->list (current-command-line-arguments)))))
          127 #| deadcode, command cannot be found |#))))

(define shell-cpp : (-> Path Symbol Any)
  (lambda [path.c lang-name]
    (shell-c path.c lang-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'exec #:shell shell~~exec
                      #:desc "Run source file (of Racket, C, and C++)"))
