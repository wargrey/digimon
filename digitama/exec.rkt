#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/system)

(require "../dtrace.rkt")
(require "../filesystem.rkt")
(require "../port.rkt")

(require "minimal/system.rkt")
(require "minimal/string.rkt")
(require "minimal/symbol.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:recon exn () #:constructor-name make-exn:recon)
(struct exn:exec exn:fail:user () #:constructor-name make-exn:exec)

(define exec-abort : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:recon (~string msgfmt argl)
                           (current-continuation-marks)))))

(define exec-throw : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:exec (~string msgfmt argl)
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
                              #:/dev/stdin (Option Input-Port) #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port)
                              #:stdin-log-level (Option Symbol) #:stdin-line-limit (Option Natural) #:stdout-line-limit (Option Natural)
                              #:pre-fork (-> Void) #:post-fork (-> Void) #:atexit (-> Void))
                             Void)
  (let ([stdout-void (λ [[line : String] [v : Void]] : Void v)])
    (lambda [#:silent [silents null] #:env [alt-env #false]
             #:/dev/stdin [/dev/stdin #false] #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
             #:stdin-log-level [log-level 'note] #:stdin-line-limit [stdin-line-limit #false] #:stdout-line-limit [stdout-line-limit #false]
             #:pre-fork [pre-fork void] #:post-fork [post-fork void] #:atexit [atexit void]
           operation program arguments [on-error-do #false]]
      ((inst fg-recon-exec* Void)
       #:silent silents #:env alt-env
       #:/dev/stdin /dev/stdin #:/dev/stdout /dev/stdout #:/dev/stderr /dev/stderr
       #:stdin-log-level log-level #:stdin-line-limit stdin-line-limit #:stdout-line-limit stdout-line-limit
       #:pre-fork pre-fork #:post-fork post-fork #:atexit atexit
       operation program arguments stdout-void (void) on-error-do))))

(define fg-recon-exec* : (All (a) (->* (Any Path Exec-Arguments (-> String a a) a)
                                       ((Option Exec-Error-Handler)
                                        #:silent (Listof Exec-Silent) #:env Exec-Alt-Env
                                        #:/dev/stdin (Option Input-Port) #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port)
                                        #:stdin-log-level (Option Symbol) #:stdin-line-limit (Option Natural) #:stdout-line-limit (Option Natural)
                                        #:pre-fork (-> Void) #:post-fork (-> Void) #:atexit (-> Void))
                                       a))
  (lambda [#:silent [silents null] #:env [alt-env #false]
           #:/dev/stdin [/dev/stdin #false] #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           #:pre-fork [pre-fork void] #:post-fork [post-fork void] #:atexit [atexit void]
           #:stdin-log-level [log-level 'note] #:stdin-line-limit [stdin-line-limit #false] #:stdout-line-limit [stdout-line-limit #false]
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
          (define-values (/usr/bin/$0 /dev/outin /dev/frkout /dev/errin)
            (fg-recon-fork operation program arguments alt-env pre-fork))

          (post-fork)

          (define ghostcat : Thread
            (thread (λ [] (when (or /dev/stdin)
                            (fg-recon-copy-port /usr/bin/$0 /dev/stdin /dev/frkout
                                                (and (not stdin-silent?) log-level)
                                                operation stdin-line-limit)))))

          (define final-datum : a
            (let wait-fold-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin]
                                 [line-count : Natural 0]
                                 [datum : a initial-datum])
              (if (not (eq? outin-evt errin-evt))
                  (let ([e (sync/enable-break outin-evt errin-evt)])             
                    (if (eq? e /dev/outin)
                        (let ([line (read-line /dev/outin 'any)]
                              [display? (if (and stdout-line-limit) (< line-count stdout-line-limit) #true)])
                          (if (string? line)
                              (begin (when (and /dev/stdout display?)
                                       (displayln line /dev/stdout)
                                       (flush-output /dev/stdout))
                                     (when (and (not stdout-silent?) display?)
                                       (dtrace-note line #:topic operation #:prefix? #false))
                                     (wait-fold-loop outin-evt errin-evt (add1 line-count) (stdout-fold line datum)))
                              (wait-fold-loop never-evt errin-evt line-count datum)))
                        (let ([line (read-line /dev/errin 'any)])
                          (if (string? line)
                              (begin (unless (not /dev/stderr)
                                       (displayln line /dev/stderr)
                                       (flush-output /dev/stderr))
                                     (if (not stderr-silent?)
                                         (dtrace-error line #:topic operation #:prefix? #false)
                                         (displayln line /dev/byterr))
                                     (wait-fold-loop outin-evt errin-evt line-count datum))
                              (wait-fold-loop outin-evt never-evt line-count datum)))))

                  ; implies both `always-evt`
                  datum)))

          (subprocess-wait /usr/bin/$0)
          (atexit)
          (thread-wait ghostcat)
          (values (subprocess-status /usr/bin/$0) final-datum)))

      (fg-recon-handle-status operation program status /dev/byterr on-error-do)
      datum:out)))

(define fg-recon-exec/pipe : (->* (Any Path Exec-Arguments)
                                  ((Option Exec-Error-Handler)
                                   #:env Exec-Alt-Env #:pre-fork (-> Void) #:post-fork (-> Void) #:atexit (-> Void)
                                   #:/dev/stdin (Option Input-Port) #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port)
                                   #:stdin-log-level (Option Symbol) #:stdin-line-limit (Option Natural) #:stdout-line-limit (Option Natural))
                                  Bytes)
  (lambda [#:env [alt-env #false] #:pre-fork [pre-fork void] #:post-fork [post-fork void] #:atexit [atexit void]
           #:/dev/stdin [/dev/stdin #false] #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           #:stdin-log-level [log-level #false] #:stdin-line-limit [stdin-line-limit #false] #:stdout-line-limit [stdout-line-limit #false]
           operation:any program arguments [on-error-do #false]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define operation : Symbol (datum-name operation:any #:no-prefix-for-symbol-datum? #true))
      (define /dev/bytout : Output-Port (open-output-bytes))
      (define /dev/byterr : Output-Port (open-output-bytes))

      (define status
        (with-handlers ([exn? (λ [[e : exn]] e)])
          (define-values (/usr/bin/$0 /dev/outin /dev/frkout /dev/errin)
            ; because subprocess only accepts file stream ports as pipes
            (fg-recon-fork operation program arguments alt-env pre-fork))

          (post-fork)

          (define-values (ghostcat/out ghostcat/err ghostcat/frkout)
            (values (thread (λ [] (if (not stdout-line-limit)
                                      (when (input-port? /dev/outin)
                                        (port-copy /dev/outin /dev/bytout /dev/stdout))
                                      (let port-copy-limited ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                                              [line-count : Natural 0])
                                        (define line (read-line /dev/outin 'any))
                                        (define display? (if (and stdout-line-limit) (< line-count stdout-line-limit) #true))

                                        (when (string? line)
                                          (when (and /dev/stdout display?)
                                            (displayln line /dev/stdout)
                                            (flush-output /dev/stdout))
                                          (displayln line /dev/bytout)
                                          (port-copy-limited outin-evt (add1 line-count)))))))
                    (thread (λ [] (when (input-port? /dev/errin)
                                    (port-copy /dev/errin /dev/byterr /dev/stderr))))
                    (thread (λ [] (when (and /dev/stdin /dev/frkout)
                                    (fg-recon-copy-port /usr/bin/$0 /dev/stdin /dev/frkout log-level operation stdin-line-limit))))))

          (thread-wait ghostcat/out)
          (thread-wait ghostcat/err)
          (subprocess-wait /usr/bin/$0)
          (atexit)
          (thread-wait ghostcat/frkout)
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
(define fg-recon-fork : (-> Symbol Path Exec-Arguments Exec-Alt-Env (-> Void)
                            (Values Subprocess Input-Port Output-Port Input-Port))
  (lambda [operation program arguments alt-env pre-fork]
    (define args : (Listof String)
      (if (vector? arguments)
          (vector->list arguments)
          (apply append arguments)))

    (define subenv : (Option Environment-Variables)
      (cond [(not alt-env) #false]
            [(environment-variables? alt-env) alt-env]
            [else (alt-env)]))
    
    (dtrace-info #:topic operation "~a ~a" program (string-join args))

    (pre-fork)
    (cond [(not subenv) (apply subprocess #false #false #false program args)]
          [else (parameterize ([current-environment-variables subenv])
                  (apply subprocess #false #false #false program args))])))

(define fg-recon-copy-port : (-> Subprocess Input-Port Output-Port (Option Symbol) Any (Option Natural) Void)
  (lambda [bin /dev/stdin /dev/frkout log-level operation max-line-of-stdin]
    (define /dev/dtout (and log-level (open-output-dtrace log-level operation #:line-limit max-line-of-stdin)))

    (port-copy/usrin #:done? (λ [] (exact-integer? (subprocess-status bin)))
                     /dev/stdin /dev/frkout /dev/dtout)
    
    (close-output-port /dev/frkout)

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
                 (exec-throw "~a -> ~a" program status)
                 (exec-throw "~a -> ~a~npossible reason: ~a" program status maybe-errmsg)))]
          [(exn? status) (fg-recon-handler operation status (λ [] (custodian-shutdown-all (current-custodian))))]
          [else (custodian-shutdown-all (current-custodian))])))
