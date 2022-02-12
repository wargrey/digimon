#lang typed/racket/base

(provide (all-defined-out))
(provide wisemon-spec? wisemon-spec-ref)
(provide Wisemon-Spec Wisemon-Specification)

(require racket/future)

(require "digitama/wisemon.rkt")
(require "digitama/exec.rkt")

(require "filesystem.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (wisemon-spec stx)
  (syntax-parse stx #:datum-literals []
    [(_ target #:^ prerequisites #:- expr ...)
     (syntax/loc stx
       (unsafe-wisemon-spec target prerequisites
                            (λ [[$@ : Path] [$? : (Listof Path)]]
                              expr ... (void))))]
    [(_ target #:^ prerequisites #:$ [$@ $?] expr ...)
     (syntax/loc stx
       (unsafe-wisemon-spec target prerequisites
                            (λ [[$@ : Path] [$? : (Listof Path)]]
                              expr ... (void))))]
    [(_ target #:- expr ...)
     (syntax/loc stx (wisemon-spec target #:^ null #:- expr ...))]
    [(_ target #:$ [$@ $?] expr ...)
     (syntax/loc stx (wisemon-spec target #:^ null #:$ [$@ $?] expr ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Specification)
                            ((U (Listof Path-String) Regexp)
                             #:keep-going? Boolean #:dry-run? Boolean #:always-run? Boolean #:just-touch? Boolean
                             #:jobs Positive-Integer #:name Symbol #:cd Path-String
                             #:assume-old (Listof Path-String) #:assume-new (Listof Path-String))
                            Void)
  (lambda [specs [targets null]
                 #:keep-going? [keep-going? #false] #:dry-run? [dry-run? #false] #:always-run? [always-run? #false] #:just-touch? [just-touch? #false]
                 #:jobs [jobs (processor-count)] #:name [the-name 'wisemon] #:cd [cd (current-directory)]
                 #:assume-old [oldfiles null] #:assume-new [newfiles null]]
    (when (or (pair? specs) (pair? targets) (regexp? targets))
      (parameterize ([current-directory cd])
        (define all-targets : (Listof Path)
          (cond [(pair? targets) (map simple-form-path targets)]
                [(regexp? targets) (wisemon-targets-flatten specs targets)]
                [else (wisemon-targets-flatten specs #false)]))

        (for ([t (in-list all-targets)])
          (with-handlers ([exn:wisemon? (λ [[e : exn:wisemon]] (if (not keep-going?) (raise e) (dtrace-warn-exception the-name e)))])
            (wisemon-make-target specs t the-name dry-run? always-run? just-touch?
                                 (map simple-form-path oldfiles)
                                 (map simple-form-path newfiles))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-targets-flatten : (->* (Wisemon-Specification) ((Option Regexp)) (Listof Path))
  (lambda [specs [re #false]]
    (for/fold ([targets : (Listof Path) null])
              ([spec (in-list specs)])
      (define ts (wisemon-spec-target spec))
      (if (not re)
          (cond [(list? ts) (append targets ts)]
                [else (append targets (list ts))])
          (cond [(list? ts) (append targets (for/list : (Listof Path) ([t (in-list ts)] #:when (regexp-match? re t)) t))]
                [(regexp-match? re ts) (append targets (list ts))]
                [else targets])))))

(define wisemon-spec-target-exists? : (->* (Wisemon-Spec) (Boolean) Boolean)
  (lambda [spec [must-all? #false]]
    (define targets : Wisemon-Targets (wisemon-spec-target spec))

    (cond [(path? targets) (file-exists? targets)]
          [(not must-all?) (ormap file-exists? targets)]
          [else (andmap file-exists? targets)])))

(define wisemon-spec->clean-spec : (->* (Wisemon-Spec) (Symbol) Wisemon-Spec)
  (lambda [spec [operation 'clean]]
    (wisemon-path->clean-spec (wisemon-spec-target spec) operation)))

(define wisemon-path->clean-spec : (->* (Wisemon-Targets) (Symbol) Wisemon-Spec)
  (lambda [path [operation 'clean]]
    (wisemon-spec path #:$ [$@ $?]
                  (fg-recon-rm operation $@))))
