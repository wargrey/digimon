#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require racket/path)

(require "../problem.rkt")

(require "../../../token.rkt")
(require "../../../string.rkt")
(require "../../../spec.rkt")

(require "../../../digitama/exec.rkt")
(require "../../../digitama/spec/dsl.rkt")
(require "../../../digitama/spec/behavior.rkt")

(require "../../../digitama/collection.rkt")
(require "../../../digitama/minimal/dtrace.rkt")
(require "../../../digitama/minimal/dtrecho.rkt")

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
(define read-lean-problem-info : (-> Path (Option Problem-Info))
  (lambda [main.lean]
    (call-with-input-file* main.lean
      (λ [[/dev/stdin : Input-Port]]
        (syn-token-skip-shebang-line /dev/stdin)
        
        (let try-next-docstring : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)

          (cond [(regexp-try-match #px"^/-+" /dev/stdin)
                 (let*-values ([(head continue?) (read-lean-problem-title /dev/stdin)]
                               [(body doctests) (if (not continue?) (values null null) (read-lean-problem-description /dev/stdin))]
                               [(args results rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec main.lean rest)])
                   (if (or (pair? specs) (pair? doctests))
                       (make-problem-info head description args results specs #false)
                       (try-next-docstring)))]
                [(regexp-try-match #px"^\\s*--+" /dev/stdin)
                 (read-line /dev/stdin 'any)
                 (try-next-docstring)]
                [else #false]))))))

(define lean-problem->feature : (-> Problem-Info Path String (Vectorof String) Problem-Config Spec-Feature)
  (lambda [problem-info lean main.lean cmd-argv spec->config]
    (describe ["~a" (or (problem-info-title problem-info) main.lean)]
      #:do (for/spec ([t (in-list (problem-info-specs problem-info))])
             (define-values (usr-args result) (values (problem-spec-input t) (problem-spec-output t)))
             (define brief (problem-spec-brief t))
             (define timeout (or (problem-spec-timeout t) 0))
             (if (and (string-blank? usr-args) (not result))
                 (it brief #:do #;(pending))
                 (it brief #:do #:millisecond timeout
                   #:do (expect-stdout lean (lean-cmd-args main.lean cmd-argv) usr-args (or result null)
                                       (spec->config t))))))))

(define lean-interpreter : (-> Path (Option Path))
  (lambda [main.lean]
    (find-executable-path "lean")))

(define lean-cmd-args : (-> String (Vectorof String) (Vectorof String))
  (lambda [main.lean cmd-argv]
    (vector-append (vector "--quiet" "--run" main.lean)
                   cmd-argv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-lean-problem-title : (-> Input-Port (Values (Option String) Boolean))
  (lambda [/dev/stdin]
    (define self (read-line /dev/stdin 'any))
    
    (cond [(or (eof-object? self) (regexp-match? #px"-+/" self)) (values #false #false)]
          [(string-blank? self) (values #false #true)]
          [else (values (string-trim self) #true)])))

(define read-lean-problem-description : (-> Input-Port (Values (Listof String) (Listof String)))
  (lambda [/dev/stdin]
    ;;; TODO: Lean4 allows nested block comments
    (let read-desc ([senil : (Listof String) null]
                    [stsetcod : (Listof String) null])
      (define self (read-line /dev/stdin 'any))

      (if (string? self)
          (cond [(regexp-match? #px"-+/" self) (values (reverse senil) (reverse stsetcod))]
                [(regexp-match? #px"^\\s*--+" self) (read-desc senil stsetcod)]
                [else (read-desc (cons (regexp-replace #px"`@(\\w+)`" self "@\\1") senil) stsetcod)])
          (values (reverse senil) (reverse stsetcod))))))
