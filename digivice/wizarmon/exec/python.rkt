#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require digimon/spec)

(require digimon/digitama/exec)
(require digimon/digitama/collection)
(require digimon/digitama/minimal/dtrace)
(require digimon/digitama/minimal/dtrecho)
(require digimon/digitama/minimal/system)
(require digimon/digitama/toolchain/spec/python)

(require "../parameter.rkt")
(require "../problem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-python : (-> Path Symbol Natural)
  (lambda [mod.py lang-name]
    (define python : (Option Path) (python-interpreter mod.py))
    
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only mod.py)
                                  (current-directory))))

    (if (and python)
        (let ([problem-info (problem-info-merge (read-python-problem-info mod.py) (and maybe-info (read-problem-info mod.py maybe-info)))]
              [cmd-argv (current-command-line-arguments)])    
          (if (not problem-info)
              (shell-env-python null python (path->string mod.py) cmd-argv #false)
              (shell-env-python problem-info python (path->string mod.py) cmd-argv 'debug)))
        127 #| command not found |#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-env-python : (-> (U Problem-Info (Listof String)) Path String (Vectorof String) (Option Dtrace-Level) Natural)
  (lambda [maybe-problem-info python mod.py shell-argv stdin-log-level]
    (if (list? maybe-problem-info)

        (let ([&status : (Boxof Natural) (box 0)])
          (dtrace-sync)
          (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level stdin-log-level
                              #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                              'exec python (python-cmd-args mod.py shell-argv (pair? maybe-problem-info) (wizarmon-verbose)) &status)
          (dtrace-sync)
          (unbox &status))

        (let ([specs (problem-info-specs maybe-problem-info)])
          (dtrace-problem-info maybe-problem-info)
          
          (if (pair? specs)
              (spec-prove #:no-timing-info? #true #:no-location-info? #true #:no-argument-expression? #true #:timeout (wizarmon-spec-timeout)
                          #:pre-spec dtrace-sync #:post-spec dtrace-sync #:post-behavior dtrace-sync
                          (python-problem->feature maybe-problem-info python mod.py shell-argv
                                                   (make-spec-problem-config stdin-log-level)
                                                   (wizarmon-verbose)))
              (shell-env-python (python-problem-attachment-doctests (assert (problem-info-attachment maybe-problem-info) python-problem-attachment?))
                                python mod.py shell-argv stdin-log-level))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define python-interpreter : (->* () ((Option Path)) (Option Path))
  (lambda [[mod.py #false]]
    (define python3 (find-executable-path "python3"))
    (define python (or python3 (find-executable-path "python")))

    (or python
        (let ([exe (format "C:\\Users\\~a\\AppData\\Local\\Microsoft\\WindowsApps\\python3.exe" digimon-partner)])
          (and (file-exists? exe)
               (string->path exe))))))
