#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/system)

(require "../dtrace.rkt")
(require "../filesystem.rkt")
(require "../port.rkt")
(require "../format.rkt")
(require "../symbol.rkt")

(require "minimal/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:recon exn () #:constructor-name make-exn:recon)

(define exec-abort : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:recon (~string msgfmt argl)
                           (current-continuation-marks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Exec-Silent (U 'stdin 'stdout 'stderr))
(define-type Exec-Alt-Env (U False Environment-Variables (-> Environment-Variables)))

(define-type Exec-Error-Handler (U (-> Symbol Path Natural Bytes Void) (Boxof Nonnegative-Integer)))
(define-type Exec-Arguments (U (Listof (Listof String)) (Vectorof String)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-recon-exec : (->* (Symbol Path Exec-Arguments)
                             ((Option Exec-Error-Handler)
                              #:silent (Listof Exec-Silent) #:env Exec-Alt-Env
                              #:/dev/stdin (Option Input-Port) #:stdin-log-level (Option Symbol)
                              #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port))
                             Void)
  (let ([stdout-void (λ [[line : String] [v : Void]] : Void v)])
    (lambda [#:silent [silents null] #:env [alt-env #false]
             #:/dev/stdin [/dev/stdin #false] #:stdin-log-level [log-level 'note]
             #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
             operation program arguments [on-error-do #false]]
      ((inst fg-recon-exec* Void)
       #:silent silents #:env alt-env
       #:/dev/stdin /dev/stdin #:stdin-log-level log-level
       #:/dev/stdout /dev/stdout #:/dev/stderr /dev/stderr
       operation program arguments stdout-void (void) on-error-do))))

(define fg-recon-exec* : (All (a) (->* (Any Path Exec-Arguments (-> String a a) a)
                                       ((Option Exec-Error-Handler)
                                        #:silent (Listof Exec-Silent) #:env Exec-Alt-Env
                                        #:/dev/stdin (Option Input-Port) #:stdin-log-level (Option Symbol)
                                        #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port))
                                       a))
  (lambda [#:silent [silents null] #:env [alt-env #false]
           #:/dev/stdin [/dev/stdin #false] #:stdin-log-level [log-level 'note]
           #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           operation:any program arguments stdout-fold initial-datum [on-error-do #false]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define operation : Symbol (datum-name operation:any #:no-prefix-for-symbol-datum? #true))
      (define /dev/byterr : Output-Port (open-output-bytes))
      (define stdin-silent? : Boolean (and (memq 'stdin silents) #true))
      (define stdout-silent? : Boolean (and (memq 'stdout silents) #true))
      (define stderr-silent? : Boolean (and (memq 'stderr silents) #true))
      
      (define-values (status datum:out)
        (with-handlers ([exn? (λ [[e : exn]] (values e initial-datum))])
          (define-values (/usr/bin/$0 /dev/outin /dev/subout /dev/errin)
            (fg-recon-fork operation program arguments alt-env))

          (define ghostcat : Thread
            (thread (λ [] (when (or /dev/stdin)
                            (fg-recon-copy-port /usr/bin/$0 /dev/stdin /dev/subout
                                                (and (not stdin-silent?) log-level)
                                                operation)))))

          (define final-datum : a
            (let wait-fold-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin]
                                 [datum : a initial-datum])
              (if (not (eq? outin-evt errin-evt))
                  (let ([e (sync/enable-break outin-evt errin-evt)])             
                    (if (eq? e /dev/outin)
                        (let ([line (read-line /dev/outin 'any)])
                          (if (eof-object? line)
                              (wait-fold-loop never-evt errin-evt datum)
                              (begin (unless (not /dev/stdout)
                                       (displayln line /dev/stdout)
                                       (flush-output /dev/stdout))
                                     (when (not stdout-silent?)
                                       (dtrace-note line #:topic operation #:prefix? #false))
                                     (wait-fold-loop outin-evt errin-evt (stdout-fold line datum)))))
                        (let ([line (read-line /dev/errin 'any)])
                          (if (eof-object? line)
                              (wait-fold-loop outin-evt never-evt datum)
                              (begin (unless (not /dev/stderr)
                                       (displayln line /dev/stderr)
                                       (flush-output /dev/stderr))
                                     (if (not stderr-silent?)
                                         (dtrace-error line #:topic operation #:prefix? #false)
                                         (displayln line /dev/byterr))
                                     (wait-fold-loop outin-evt errin-evt datum))))))

                  ; implies both `always-evt`
                  datum)))

          (subprocess-wait /usr/bin/$0)
          (thread-wait ghostcat)
          (values (subprocess-status /usr/bin/$0) final-datum)))

      (fg-recon-handle-status operation program status /dev/byterr on-error-do)
      datum:out)))

(define fg-recon-exec/pipe : (->* (Any Path Exec-Arguments)
                                  ((Option Exec-Error-Handler)
                                   #:env Exec-Alt-Env #:/dev/stdin (Option Input-Port) #:stdin-log-level (Option Symbol)
                                   #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port))
                                  Bytes)
  (lambda [#:env [alt-env #false] #:/dev/stdin [/dev/stdin #false] #:stdin-log-level [log-level #false]
           #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           operation:any program arguments [on-error-do #false]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define operation : Symbol (datum-name operation:any #:no-prefix-for-symbol-datum? #true))
      (define /dev/bytout : Output-Port (open-output-bytes))
      (define /dev/byterr : Output-Port (open-output-bytes))

      (define status
        (with-handlers ([exn? (λ [[e : exn]] e)])
          (define-values (/usr/bin/$0 /dev/outin /dev/subout /dev/errin)
            ; because subprocess only accepts file stream ports as pipes
            (fg-recon-fork operation program arguments alt-env))

          (define-values (ghostcat/out ghostcat/err ghostcat/subout)
            (values (thread (λ [] (when (input-port? /dev/outin)
                                    (port-copy /dev/outin /dev/bytout /dev/stdout))))
                    (thread (λ [] (when (input-port? /dev/errin)
                                    (port-copy /dev/errin /dev/byterr /dev/stderr))))
                    (thread (λ [] (when (and /dev/stdin /dev/subout)
                                    (fg-recon-copy-port /usr/bin/$0 /dev/stdin /dev/subout log-level operation))))))

          (thread-wait ghostcat/out)
          (thread-wait ghostcat/err)
          (subprocess-wait /usr/bin/$0)
          (thread-wait ghostcat/subout)
          (subprocess-status /usr/bin/$0)))

      (fg-recon-handle-status operation program status /dev/byterr on-error-do)
      (get-output-bytes /dev/bytout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-eval : (->* (Symbol Any) (Namespace) AnyValues)
  (lambda [operation s-expr [ns (current-namespace)]]
    (dtrace-info #:topic operation "eval: ~a" s-expr)
    (eval s-expr ns)))

(define fg-recon-eval : (->* (Symbol Any) (Namespace) AnyValues)
  (lambda [operation s-expr [ns (current-namespace)]]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-eval operation s-expr ns))))

(define fg-recon-save-file : (All (a) (-> Symbol Path-String (-> Output-Port a) (U a Void)))
  (lambda [operation path write]
    (dtrace-info #:topic operation "(call-to-save-file ~a ~a)" path (object-name write))
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (call-with-output-file* path write #:exists 'truncate/replace))))

(define fg-recon-open-file : (-> Symbol Path-String Void)
  (lambda [operation file]
    (when (file-exists? file)
      (dtrace-info #:topic operation "open ~a" file)

      (case digimon-system
        [(windows) (shell-execute "open" (if (path? file) (path->string file) file) "" (current-directory) 'sw_shownormal)]
        [else (let ([open (find-executable-path "open")])
                (when (or open) (system*/exit-code open file)))])
      
      (void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-mv : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (unless (and (file-exists? dest)
                 (equal? (file-or-directory-identity dest)
                         (file-or-directory-identity src)))
      (dtrace-info #:topic operation "mv ~a ~a" src dest)
      (rename-file-or-directory src dest #true))))

(define fg-recon-mv : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-mv operation src dest))))

(define fg-cp : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (file-touch dest)

    (unless (equal? (file-or-directory-identity dest)
                    (file-or-directory-identity src))
      (dtrace-info #:topic operation "cp ~a ~a" src dest)
      (delete-file dest)
      (copy-file src dest))))

(define fg-recon-cp : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-cp operation src dest))))

(define fg-mkdir : (-> Symbol Path-String Void)
  (lambda [operation dir]
    (dtrace-info #:topic operation "mkdir -p ~a" dir)
    (make-directory* dir)))

(define fg-recon-mkdir : (-> Symbol Path-String Void)
  (lambda [operation dir]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-mkdir operation dir))))

(define fg-touch : (-> Symbol Path-String Void)
  (lambda [operation file]
    (dtrace-info #:topic operation "touch ~a" file)
    (file-touch file)))

(define fg-recon-touch : (-> Symbol Path-String Void)
  (lambda [operation file]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-touch operation file))))

(define fg-rm : (-> Symbol Path-String [#:fr? Boolean] Void)
  (lambda [operation target #:fr? [fr? #false]]
    (when (or (file-exists? target) (directory-exists? target))
      (if (not fr?)
          (dtrace-info #:topic operation "rm ~a" target)
          (dtrace-info #:topic operation "rm -fr ~a" target))
      
      (cond [(file-exists? target) (delete-file target)]
            [(not fr?) (delete-directory target)]
            [else (delete-directory/files target)]))))

(define fg-recon-rm : (-> Symbol Path-String [#:fr? Boolean] Void)
  (lambda [operation file #:fr? [fr? #false]]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-rm operation file #:fr? fr?))))

(define fg-cat : (->* (Symbol Path-String) (Output-Port) Void)
  (lambda [operation file [/dev/stdout (current-output-port)]]
    (dtrace-info #:topic operation "cat ~a" file)
    
    (call-with-input-file* file
      (λ [[/dev/stdin : Input-Port]]
        (port-copy /dev/stdin /dev/stdout)))))

(define fg-recon-cat : (->* (Symbol Path-String) (Output-Port) Void)
  (lambda [operation file [/dev/stdout (current-output-port)]]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-cat operation file /dev/stdout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-recon-fork : (-> Symbol Path Exec-Arguments Exec-Alt-Env
                            (Values Subprocess Input-Port Output-Port Input-Port))
  (lambda [operation program arguments alt-env]
    (define args : (Listof String)
      (if (vector? arguments)
          (vector->list arguments)
          (apply append arguments)))

    (define subenv : (Option Environment-Variables)
      (cond [(not alt-env) #false]
            [(environment-variables? alt-env) alt-env]
            [else (alt-env)]))
    
    (dtrace-info #:topic operation "~a ~a" program (string-join args))
    
    (cond [(not subenv) (apply subprocess #false #false #false program args)]
          [else (parameterize ([current-environment-variables subenv])
                  (apply subprocess #false #false #false program args))])))

(define fg-recon-copy-port : (-> Subprocess Input-Port Output-Port (Option Symbol) Any Void)
  (lambda [bin /dev/stdin /dev/subout log-level operation]
    (define /dev/dtout (and log-level (open-output-dtrace log-level operation)))

    (port-copy/usrin #:done? (λ [] (exact-integer? (subprocess-status bin)))
                     /dev/stdin /dev/subout /dev/dtout)
    
    (close-output-port /dev/subout)

    (when (and /dev/dtout)
      (close-output-port /dev/dtout))))

(define fg-recon-handler : (->* (Symbol exn) ((-> Void)) Void)
  (lambda [operation e [clean void]]
    (cond [(exn:recon? e) (dtrace-note #:topic operation (exn-message e))]
          [(exn:fail? e) (clean) (raise e)]
          [else #;(exn:break? e) (clean)])))

(define fg-recon-handle-status : (-> Symbol Path (U Natural exn 'running) Output-Port (Option Exec-Error-Handler) Void)
  (lambda [operation program status /dev/byterr on-error-do]
    (cond [(exact-integer? status)
           (unless (eq? status 0)
             (define maybe-errmsg (get-output-bytes /dev/byterr))
             
             (cond [(box? on-error-do) (set-box! on-error-do status)]
                   [(and on-error-do) (on-error-do operation program status maybe-errmsg)])
             
             (custodian-shutdown-all (current-custodian))

             (if (= (bytes-length maybe-errmsg) 0)
                 (raise-user-error operation "~a -> ~a" program status)
                 (raise-user-error operation "~a -> ~a~npossible reason: ~a" program status maybe-errmsg)))]
          [(exn? status) (fg-recon-handler operation status (λ [] (custodian-shutdown-all (current-custodian))))]
          [else (custodian-shutdown-all (current-custodian))])))
