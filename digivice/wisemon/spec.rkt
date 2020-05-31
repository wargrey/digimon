#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../../wisemon.rkt"))
(provide file-or-directory-modify-seconds)

(require racket/file)
(require racket/path)
(require racket/format)

(require typed/racket/unsafe)

(require "parameter.rkt")
(require "../../wisemon.rkt")

(unsafe-require/typed
 racket/base
 [file-or-directory-modify-seconds (All (a) (-> Path-String (Option Integer) (-> a) a))])

(unsafe-require/typed/provide
 make
 [make/proc (-> Make-Specification (U String (Listof Path-String) (Vectorof Path-String)) Void)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Make-Spec (List (U Path (Listof Path)) (Listof Path) (-> Void)))
(define-type Make-Specification (Listof Make-Spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Specification) ((Listof Path-String)) Void)
  (lambda [specs [targets null]]
    (when (pair? specs)
      (define make-specs (map hack-spec specs))
      (when (pair? make-specs)
        (make/proc make-specs
                   (cond [(pair? targets) targets]
                         [else (wisemon-targets-flatten specs)]))))))

(define wisemon-targets-flatten : (-> Wisemon-Specification (Listof Path))
  (lambda [specs]
    (for/fold ([targets : (Listof Path) null])
              ([spec (in-list specs)])
      (define ts (wisemon-spec-target spec))
      (cond [(list? ts) (append targets ts)]
            [else (append targets (list ts))]))))

(define wisemon-spec-ref : (-> Wisemon-Specification Path-String (Option Wisemon-Spec))
  (lambda [specs target]
    (define t (simple-form-path target))
    (findf (λ [[s : Wisemon-Spec]] : Boolean
             (let ([ts (wisemon-spec-target s)])
               (cond [(list? ts) (and (member t ts) #true)]
                     [else (equal? t ts)])))
           specs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hack-spec : (-> Wisemon-Spec Make-Spec)
  (lambda [spec]
    (define targets (wisemon-spec-target spec))
    (define prerequisites (wisemon-spec-prerequisites spec))
    (define recipe
      (λ [] (for ([target (if (list? targets) (in-list targets) (in-value targets))])
              (let* ([t-already-exists? (file-exists? target)]
                     [tmp (make-temporary-file (~a (file-name-from-path target) ".~a") (and t-already-exists? target))])
                (dynamic-wind (λ [] (make-parent-directory* target))
                              (λ [] (void ((wisemon-spec-recipe spec) target)))
                              (λ [] (when (make-dry-run)
                                      (cond [t-already-exists? (rename-file-or-directory tmp target #true)]
                                            [(file-exists? target) #| now exists |# (delete-file target)]))))))))
    
    (list targets (if (make-always-run) (cons (current-directory) prerequisites) prerequisites)
          (cond [(not (make-just-touch)) recipe]
                [else (λ [] (for ([target (if (list? targets) (in-list targets) (in-value targets))])
                              (file-or-directory-modify-seconds target (current-seconds) recipe)))]))))
