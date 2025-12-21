#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require digimon/spec)

(require digimon/digitama/exec)
(require digimon/digitama/collection)
(require digimon/digitama/minimal/dtrace)
(require digimon/digitama/minimal/dtrecho)
(require digimon/digitama/toolchain/spec/lean)

(require "../problem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-lean : (-> Path Symbol Natural)
  (lambda [main.lean lang-name]
    (define lean : (Option Path) (lean-interpreter main.lean))

    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only main.lean)
                                  (current-directory))))

    (if (and lean)
        (let ([problem-info (problem-info-merge (read-lean-problem-info main.lean) (and maybe-info (read-problem-info main.lean maybe-info)))]
              [cmd-argv (current-command-line-arguments)])    
          (if (not problem-info)
              (shell-env-lean null lean (path->string main.lean) cmd-argv #false)
              (shell-env-lean problem-info lean (path->string main.lean) cmd-argv 'debug)))
        127 #| command not found |#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-env-lean : (-> (U Problem-Info (Listof String)) Path String (Vectorof String) (Option Dtrace-Level) Natural)
  (lambda [maybe-problem-info lean main.lean cmd-argv stdin-log-level]
    (if (list? maybe-problem-info)

        (let ([&status : (Boxof Natural) (box 0)])
          (dtrace-sync)
          (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level stdin-log-level
                              #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                              'exec lean (lean-cmd-args main.lean cmd-argv) &status)
          (dtrace-sync)
          (unbox &status))

        (let ([specs (problem-info-specs maybe-problem-info)])
          (dtrace-problem-info maybe-problem-info)
          (spec-prove #:no-timing-info? #true #:no-location-info? #true #:no-argument-expression? #true #:timeout (wizarmon-spec-timeout)
                      #:pre-spec dtrace-sync #:post-spec dtrace-sync #:post-behavior dtrace-sync
                      (lean-problem->feature maybe-problem-info lean main.lean cmd-argv (make-spec-problem-config stdin-log-level)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lean-interpreter : (-> Path (Option Path))
  (lambda [main.lean]
    (find-executable-path "lean")))
