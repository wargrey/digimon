#lang typed/racket/base

(provide (all-defined-out))

(require "../parameter.rkt")

(require "../../wisemon/phony/cc.rkt")
(require (only-in "../../wisemon/parameter.rkt"
                  current-make-real-targets))

(require "../../../wisemon.rkt")
(require "../../../environ.rkt")
(require "../../../filesystem.rkt")
(require "../../../spec.rkt")

(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../digitama/collection.rkt")

(require "../../../digitama/toolchain/problem.rkt")
(require "../../../digitama/toolchain/spec/clang.rkt")
(require "../../../digitama/toolchain/cc/configuration.rkt")

(require "../../../digitama/minimal/dtrace.rkt")
(require "../../../digitama/minimal/dtrecho.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
              (define cmd-argv : (Vectorof String) (current-command-line-arguments))
              (if (not problem-info)
                  (shell-exec-a.out (car targets) cmd-argv #false)
                  (shell-exec-a.out problem-info (car targets) cmd-argv 'debug))))
          127 #| deadcode, command cannot be found |#))))

(define shell-cpp : (-> Path Symbol Natural)
  (lambda [path.c lang-name]
    (shell-c path.c lang-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-exec-a.out : (case-> [Problem-Info Path (Vectorof String) (Option Dtrace-Level) -> Natural]
                                   [Path (Vectorof String) (Option Dtrace-Level) -> Natural])
  (case-lambda
    [(a.out cmd-argv stdin-log-level)
     (define &status : (Boxof Nonnegative-Integer) (box 0))

     (dtrace-sync)
     (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level stdin-log-level
                         #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                         'exec a.out cmd-argv &status)
     (dtrace-sync)
     (unbox &status)]
    [(problem-info a.out cmd-argv stdin-log-level)
     (dtrace-problem-info problem-info)
     (parameterize ([default-spec-exec-stdin-log-level stdin-log-level]
                    [default-spec-exec-stdout-port (current-output-port)])
       (spec-prove #:no-timing-info? #true #:no-location-info? #true #:no-argument-expression? #true #:timeout (wizarmon-timeout)
                   #:pre-spec dtrace-sync #:post-spec dtrace-sync #:post-behavior dtrace-sync
                   (clang-problem->feature problem-info a.out cmd-argv)))]))
