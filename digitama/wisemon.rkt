#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/format)
(require racket/sandbox)

(require "../dtrace.rkt")
(require "../format.rkt")

(require "../exception.rkt")
(require "../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Specification (Listof Wisemon-Spec))

(struct wisemon-spec
  ([target : (U Path (Listof Path))]
   [prerequisites : (Listof Path)]
   [recipe : (-> Void)]
   [recon : Special-Comment])
  #:constructor-name unsafe-wisemon-spec
  #:type-name Wisemon-Spec
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Exception-Cause (U Symbol exn:fail))

(define-exception exn:wisemon exn:fail:user
  ([targets : (Listof Path)]
   [cause : Wisemon-Exception-Cause])
  (wisemon-exn-message))

(define wisemon-exn-message : (-> Any (Listof Path) Wisemon-Exception-Cause String String)
  (lambda [func targets cause message]
    (format "~a: ~a" func message)))

(define wisemon-log-message : (->* (Symbol Log-Level String) (#:targets (Listof Path)) #:rest Any Void)
  (lambda [name level #:targets [targets null] msgfmt . argl]
    (dtrace-send name level (~string msgfmt argl) targets #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make-target : (-> Wisemon-Specification Path Symbol Boolean Boolean Boolean (Listof Path) (Listof Path) Void)
  (lambda [specs target name dry-run? always-run? just-touch? oldfiles newfiles]
    (define now : Integer (current-seconds))

    (define (wisemon-mtime [file : Path]) : Integer
      ; `gmake`'s new-assumed files have higher priority regardless the order in cmdline options
      (cond [(member file newfiles) now]
            [(member file oldfiles) 0]
            [else (file-mtime file 0)]))
    
    (define (make [t : Path] [ts : (Listof Path)]) : Integer
      (define spec : (Option Wisemon-Spec) (wisemon-spec-ref specs t))
      (define targets : (Listof Path) (reverse (cons t ts)))
      
      (cond [(wisemon-spec? spec)
             (define self-mtime : Integer (wisemon-mtime t))
             (define newers : (Listof Path)
               (let check-prerequisites ([prerequisites : (Listof Path) (wisemon-spec-prerequisites spec)]
                                         [srewen : (Listof Path) null])
                 (cond [(null? prerequisites) (reverse srewen)]
                       [else (let ([prerequisite (car prerequisites)])
                               (check-prerequisites (cdr prerequisites)
                                                    (cond [(>= self-mtime (make prerequisite (cons t ts))) srewen]
                                                          [else (cons prerequisite srewen)])))])))
             (unless (member t oldfiles)
               (when (or always-run? (pair? newers))
                 (define ./target (find-relative-path (current-directory) t))
                 (define indent (~space (* (length ts) 2)))
                 
                 (for ([p (in-list newers)])
                   (wisemon-log-message name 'debug #:targets targets "~aprerequisite `~a` is newer than the target `~a`"
                                        indent (find-relative-path (current-directory) t) ./target))

                 (cond [(and always-run?) (wisemon-log-message name 'debug #:targets targets "~aremaking `~a` unconditionally" indent ./target)]
                       [(file-exists? t) (wisemon-log-message name 'debug #:targets targets "~aremaking `~a` due to outdated" indent ./target)]
                       [else (wisemon-log-message name 'debug #:targets (cons t ts) "~aremaking `~a` due to absent" indent ./target)])

                 (cond [(and just-touch?) (wisemon-log-message name 'info #:targets targets "~atouch `~a`" indent t) (file-touch t)]
                       [(not dry-run?) (wisemon-run name (wisemon-spec-recipe spec) targets)]
                       [else (wisemon-dry-run (special-comment-value (wisemon-spec-recon spec)))])

                 (wisemon-log-message name 'debug #:targets targets "~aremade `~a`" indent ./target)))
             
             (wisemon-mtime t)]
            [(file-exists? t) (wisemon-mtime t)]
            [else (throw-exn:wisemon name targets 'unregistered "no rule to make target `~a`" t)]))
    
    (void (make target null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-spec-ref : (-> Wisemon-Specification Path-String (Option Wisemon-Spec))
  (lambda [specs target]
    (define t (simple-form-path target))
    (findf (λ [[s : Wisemon-Spec]] : Boolean
             (let ([ts (wisemon-spec-target s)])
               (cond [(list? ts) (and (member t ts) #true)]
                     [else (equal? t ts)])))
           specs)))

(define wisemon-dry-run : (-> Any Void)
  (lambda [exprs]
    (when (list? exprs)
      #;(parameterize ([sandbox-output (current-output-port)]
                     [sandbox-error-output #false]
                     [sandbox-propagate-exceptions #false]
                     [sandbox-path-permissions (list (list 'read #px#".") (list 'exists #px#"."))])
        (make-evaluator 'digimon/village/hashlang/wisemon))
      (void))))

(define wisemon-run : (-> Symbol (-> Any) (Listof Path) Void)
  (lambda [name make targets]
    ;;; Racket parameters are thread-specific data.
    ;; More precisely, change a parameter out of `parameterize` does not affect the same parameter in other threads.
    ;; Here the recipe is running in a thread so that the recipe has no chance to hurt others.
    ;; `dry-run` may dislike `parameterize`.
    
    (parameterize ([current-custodian (make-custodian)])
      (define maybe-exn
        (call-in-nested-thread
         (λ [] (with-handlers ([exn:fail? values])
                 (make)))))

      (custodian-shutdown-all (current-custodian))
      
      (when (exn:fail? maybe-exn)
        (throw-exn:wisemon name targets maybe-exn (exn-message maybe-exn))))))
