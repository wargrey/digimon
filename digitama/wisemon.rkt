#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require "exec.rkt")
(require "minimal/port.rkt")
(require "minimal/string.rkt")
(require "minimal/format.rkt")
(require "minimal/dtrace.rkt")
(require "minimal/dtrecho.rkt")

(require "../echo.rkt")

(require "../exception.rkt")
(require "../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Targets (U Path (Listof Path)))
(define-type Wisemon-Specification (Listof Wisemon-Spec))

(struct wisemon-spec
  ([target : Wisemon-Targets]
   [prerequisites : (Listof Path)]
   [recipe : (-> Path (Listof Path) Void)])
  #:constructor-name unsafe-wisemon-spec
  #:type-name Wisemon-Spec
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Exception-Cause (U Symbol exn:fail))

(define-exception exn:wisemon exn:fail:user
  ([target : Path]
   [prerequisites : (Listof Path)]
   [cause : Wisemon-Exception-Cause])
  (wisemon-exn-extract))

(define wisemon-exn-extract : (-> Any Path (Listof Path) Wisemon-Exception-Cause String (Values String (Option Continuation-Mark-Set)))
  (lambda [func target prerequisites cause message]
    (values message
            (and (exn:fail? cause)
                 (exn-continuation-marks cause)))))

(define wisemon-log-message : (->* (Symbol Log-Level Path String) (#:prerequisites (Listof Path)) #:rest Any Void)
  (lambda [name level target #:prerequisites [prerequisites null] msgfmt . argl]
    (dtrace-send name level (~string msgfmt argl) (cons target prerequisites) #true)))

(define dtrace-warn-exception : (-> Symbol exn:wisemon Void)
  (lambda [name e]
    (define cause : Wisemon-Exception-Cause (exn:wisemon-cause e))
    
    (cond [(symbol? cause) (dtrace-exception e #:level 'warning #:topic name #:prefix? #false #:brief? #true)]
          [else (dtrace-exception cause #:level 'warning #:topic name #:prefix? #false #:brief? #true)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make-target : (-> Wisemon-Specification Path Symbol (HashTable Path Any) Boolean Boolean Boolean (Listof Path) (Listof Path) Void)
  (lambda [specs target name oldfiles-cache dry-run? always-run? just-touch? oldfiles newfiles]
    (define now : Integer (current-seconds))

    (define (wisemon-mtime [file : Path]) : Integer
      ; `gmake`'s new-assumed files have higher priority regardless the order in cmdline options
      (cond [(member file newfiles) now]
            [(member file oldfiles) 0]
            [else (file-mtime file 0)]))
    
    (define (make [t : Path] [ts : (Listof Path)]) : Integer
      (define spec : (Option Wisemon-Spec) (wisemon-spec-ref specs t))
      
      (cond [(wisemon-spec? spec)
             (unless (or (member t newfiles) (hash-has-key? oldfiles-cache t))
               (define no-target? : Boolean (not (file-exists? t)))
               (define self-mtime : Integer (wisemon-mtime t))
               (define newers : (Listof Path)
                 (let check-prerequisites ([prerequisites : (Listof Path) (wisemon-spec-prerequisites spec)]
                                           [srewen : (Listof Path) null])
                   (cond [(null? prerequisites) (reverse srewen)]
                         [else (let ([prerequisite (car prerequisites)]
                                     [existed-targets (cons t ts)])
                                 (when (member prerequisite existed-targets)
                                   (throw-exn:wisemon name t (reverse srewen) 'cyclic
                                                      "detected a cyclic dependency of `~a`" t))
                                 
                                 (check-prerequisites (cdr prerequisites)
                                                      (cond [(>= self-mtime (make prerequisite existed-targets)) srewen]
                                                            [else (cons prerequisite srewen)])))])))
               
               (when (or no-target? always-run? (pair? newers) (member t oldfiles))
                 (define ./target (find-relative-path (current-directory) t #:more-than-root? #true))
                 (define indent (~space (* (length ts) 2)))
                 
                 (for ([p (in-list newers)])
                   (wisemon-log-message name 'debug t #:prerequisites newers "~aprerequisite `~a` is newer than the target `~a`"
                                        indent (find-relative-path (current-directory) p #:more-than-root? #true) ./target))

                 (cond [(and always-run?) (wisemon-log-message name 'note t #:prerequisites newers "~aremaking `~a` unconditionally" indent ./target)]
                       [(or no-target?) (wisemon-log-message name 'note t #:prerequisites newers "~aremaking `~a` due to absent" indent ./target)]
                       [else (wisemon-log-message name 'note t #:prerequisites newers "~aremaking `~a` due to outdated" indent ./target)])

                 (let ([size0 (if no-target? 0 (file-size t))]
                       [ms0 (current-inexact-milliseconds)])
                   (cond [(and just-touch?) (wisemon-log-message name 'info t #:prerequisites newers "~atouch `~a`" indent t) (file-touch t)]
                         [(not dry-run?) (wisemon-run name (wisemon-spec-recipe spec) t newers)]
                         [else (wisemon-dry-run name (wisemon-spec-recipe spec) t newers)])

                   (when (or always-run? (member t oldfiles))
                     (hash-set! oldfiles-cache t #true))

                   (let ([dsize (- (file-size t) size0)]
                         [dms (- (current-inexact-milliseconds) ms0)])
                     (wisemon-log-message name 'note t #:prerequisites newers
                                          "~aremade `~a` [~ams, ~a~a]" indent ./target
                                          (~r dms #:precision 3)
                                          (cond [(= size0 0) ""]
                                                [else (~size (+ size0 dsize))])
                                          (cond [(= dsize 0) ""]
                                                [else (term-format #:fgcolor (if (> dsize 0) 'green 'red)
                                                                   "(~a~a)" (if (>= dsize 0) #\+ #\-) (~size (abs dsize)))]))))))
             
             (wisemon-mtime t)]
            [(file-exists? t) (wisemon-mtime t)]
            [else (throw-exn:wisemon name t null 'unregistered "no recipe to make target `~a`" t)]))
    
    (void (make target null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Racket parameters are thread-specific data.
;; More precisely, change a parameter out of `parameterize` does not affect the same parameter in other threads.
;; Here the recipe is running in a thread so that the recipe has no chance to hurt others.   

(define wisemon-run : (-> Symbol (-> Path (Listof Path) Any) Path (Listof Path) Void)
  (lambda [name make target prerequisites]
    (parameterize ([current-custodian (make-custodian)])
      (define maybe-exn
        (call-in-nested-thread
         (λ [] (with-handlers ([exn:fail? values])
                 (make target prerequisites)))))

      (custodian-shutdown-all (current-custodian))
      
      (when (exn:fail? maybe-exn)
        (throw-exn:wisemon name target prerequisites maybe-exn (exn-message maybe-exn))))))

(define wisemon-dry-run : (-> Symbol (-> Path (Listof Path) Any) Path (Listof Path) Void)
  (lambda [name make target prerequisites]
    (parameterize ([current-custodian (make-custodian)]
                   [current-security-guard (wisemon-security-guard)]
                   [current-output-port /dev/null]
                   [current-error-port /dev/null])
      (define maybe-exn
        (call-in-nested-thread
         (λ [] (with-handlers ([exn? values])
                 (make target prerequisites)))))

      (custodian-shutdown-all (current-custodian))
      
      #;(when (exn:fail? maybe-exn)
        (throw-exn:wisemon name target prerequisites maybe-exn (exn-message maybe-exn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-spec-ref : (-> Wisemon-Specification Path-String (Option Wisemon-Spec))
  (lambda [specs target]
    (define t (simple-form-path target))
    
    (findf (λ [[s : Wisemon-Spec]] : Boolean
             (let ([ts (wisemon-spec-target s)])
               (cond [(list? ts) (and (member t ts) #true)]
                     [else (equal? t ts)])))
           specs)))

(define wisemon-security-guard : (->* () (Security-Guard) Security-Guard)
  (lambda [[parent (current-security-guard)]]
    (make-security-guard
     parent
     (λ [[who : Symbol] [maybe-path : (Option Path)] [perms : (Listof Symbol)]]
       (when (path? maybe-path)
         (when (or (memq 'execute perms) (memq 'write perms) (memq 'delete perms))
           (exec-abort "@~a[~a]: access denied: ~a" who maybe-path perms))))
     void)))
