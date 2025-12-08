#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require racket/path)

(require digimon/token)
(require digimon/string)
(require digimon/spec)

(require digimon/digitama/exec)
(require digimon/digitama/spec/dsl)
(require digimon/digitama/spec/behavior)

(require digimon/digitama/collection)
(require digimon/digitama/minimal/dtrace)
(require digimon/digitama/minimal/dtrecho)
(require digimon/digitama/minimal/system)

(require "../parameter.rkt")
(require "../problem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct python-problem-attachment
  ([doctests : (Listof String)])
  #:constructor-name make-python-problem-attachment
  #:type-name Python-Problem-Attachment
  #:transparent)

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
  (lambda [maybe-problem-info python mod.py cmd-argv stdin-log-level]
    (if (list? maybe-problem-info)

        (let ([&status : (Boxof Natural) (box 0)])
          (dtrace-sync)
          (fg-recon-exec/pipe #:/dev/stdin (current-input-port) #:stdin-log-level stdin-log-level
                              #:/dev/stdout (current-output-port) #:/dev/stderr (current-error-port)
                              'exec python (python-cmd-args mod.py cmd-argv (pair? maybe-problem-info)) &status)
          (dtrace-sync)
          (unbox &status))

        (let ([specs (problem-info-specs maybe-problem-info)])
          (dtrace-problem-info maybe-problem-info)
          
          (if (pair? specs)
              (spec-prove #:no-timing-info? #true #:no-location-info? #true #:no-argument-expression? #true #:timeout (wizarmon-spec-timeout)
                          #:pre-spec dtrace-sync #:post-spec dtrace-sync #:post-behavior dtrace-sync
                          (python-problem->feature maybe-problem-info python mod.py cmd-argv (make-spec-problem-config stdin-log-level)))
              (shell-env-python (python-problem-attachment-doctests (assert (problem-info-attachment maybe-problem-info) python-problem-attachment?))
                                python mod.py cmd-argv stdin-log-level))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-python-problem-info : (-> Path (Option Problem-Info))
  (lambda [mod.py]
    (call-with-input-file* mod.py
      (λ [[/dev/stdin : Input-Port]]
        (syn-token-skip-shebang-line /dev/stdin)
        
        (let try-next-docstring : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)

          (define px:docstring-end : (Option Regexp)
            (cond [(regexp-try-match #px"^\"{3}" /dev/stdin) #px"\"{3}"]
                  [(regexp-try-match #px"^'{3}" /dev/stdin) #px"'{3}"]
                  [else #false]))
          
          (cond [(or px:docstring-end)
                 (let*-values ([(head continue?) (read-python-problem-title /dev/stdin px:docstring-end)]
                               [(body doctests) (if (not continue?) (values null null) (read-python-problem-description /dev/stdin px:docstring-end))]
                               [(args results rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec mod.py rest)])
                   (if (or (pair? specs) (pair? doctests))
                       (make-problem-info head description args results specs
                                          (make-python-problem-attachment doctests))
                       (try-next-docstring)))]
                [(regexp-try-match #px"^\\s*#" /dev/stdin)
                 (read-line /dev/stdin 'any)
                 (try-next-docstring)]
                [else #false]))))))

(define python-problem->feature : (-> Problem-Info Path String (Vectorof String) Problem-Config Spec-Feature)
  (lambda [problem-info python mod.py cmd-argv spec->config]
    (describe ["~a" (or (problem-info-title problem-info) mod.py)]
      #:do (for/spec ([t (in-list (problem-info-specs problem-info))])
             (define-values (usr-args result) (values (problem-spec-input t) (problem-spec-output t)))
             (define brief (problem-spec-brief t))
             (define timeout (or (problem-spec-timeout t) 0))
             (cond [(problem-spec-ignore? t)
                    (if (eq? (problem-spec-ignore? t) 'skip)
                        (if (problem-spec-reason t)
                            (it brief #:do (ignore "~a" (problem-spec-reason t)))
                            (it brief #:do (collapse "skipped test requires a reason")))
                        (if (problem-spec-reason t)
                            (it brief #:do (pending "~a" (problem-spec-reason t)))
                            (it brief #:do #;(pending))))]
                   [(and (string-blank? usr-args) (not result))
                    (it brief #:do #;(pending))]
                   [(list? result) ; a complete test spec overrides the doctests
                    (it brief #:do #:millisecond timeout
                      #:do (expect-stdout python (python-cmd-args mod.py cmd-argv #false) usr-args result (spec->config t)))]
                   [else ; let doctest do its job
                    (it brief #:do #:millisecond timeout
                      #:do (let* ([attachment (assert (problem-info-attachment problem-info) python-problem-attachment?)]
                                  [doctest? (pair? (python-problem-attachment-doctests attachment))])
                             (expect-stdout python (python-cmd-args mod.py cmd-argv doctest?) usr-args null (spec->config t))))])))))

(define python-interpreter : (->* () ((Option Path)) (Option Path))
  (lambda [[mod.py #false]]
    (define python3 (find-executable-path "python3"))
    (define python (or python3 (find-executable-path "python")))

    (or python
        (let ([exe (format "C:\\Users\\~a\\AppData\\Local\\Microsoft\\WindowsApps\\python3.exe" digimon-partner)])
          (and (file-exists? exe)
               (string->path exe))))))

(define python-cmd-args : (-> String (Vectorof String) Boolean (Vectorof String))
  (lambda [mod.py cmd-argv doctest?]
    (define pyargv : (Vectorof String) (vector "-X" "utf-8"))
    
    (cond [(not doctest?) (vector-append pyargv (vector mod.py) cmd-argv)]
          [(wizarmon-verbose) (vector-append pyargv (vector "-m" "doctest" "-v" mod.py) cmd-argv)]
          [else (vector-append pyargv (vector "-m" "doctest" mod.py) cmd-argv)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-python-problem-title : (-> Input-Port Regexp (Values (Option String) Boolean))
  (lambda [/dev/stdin px:docstring]
    (define self (read-line /dev/stdin 'any))
    
    (cond [(or (eof-object? self) (regexp-match? px:docstring self)) (values #false #false)]
          [(string-blank? self) (values #false #true)]
          [else (values (string-trim self) #true)])))

(define read-python-problem-description : (-> Input-Port Regexp (Values (Listof String) (Listof String)))
  (lambda [/dev/stdin px:docstring]
    (let read-desc ([senil : (Listof String) null]
                    [stsetcod : (Listof String) null]
                    [doctest? : Boolean #false])
      (define self (read-line /dev/stdin 'any))

      (if (string? self)
          (cond [(regexp-match? px:docstring self) (values (reverse senil) (reverse stsetcod))]
                [(regexp-match? #px"^\\s*[#]" self) (read-desc senil stsetcod doctest?)]
                [(regexp-match? #px"^\\s*[>]{3}\\s+\\S+" self) (read-desc senil (cons self stsetcod) #true)]
                [(or doctest?) (if (string-blank? self) (read-desc senil stsetcod #false) (read-desc senil (cons self stsetcod) doctest?))]
                [else (read-desc (cons self senil) stsetcod doctest?)])
          (values (reverse senil) (reverse stsetcod))))))
