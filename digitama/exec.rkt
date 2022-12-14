#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/port)

(require "../dtrace.rkt")
(require "../filesystem.rkt")
(require "../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:recon exn () #:constructor-name make-exn:recon)

(define exec-abort : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:recon (~string msgfmt argl)
                           (current-continuation-marks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Exec-Silent (U 'stdin 'stdout 'stderr))

(define fg-recon-exec* : (All (a) (->* (Symbol Path (Listof (Listof String)) (-> String a a) a)
                                       ((Option (-> Symbol Path Natural Void))
                                        #:silent (Listof Exec-Silent) #:feeds (Listof Any) #:feed-eof (U EOF String)
                                        #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port)
                                        #:env (U False Environment-Variables (-> Environment-Variables)))
                                       a))
  (lambda [#:silent [silents null] #:env [alt-env #false] #:feeds [feeds null] #:feed-eof [feed-eof eof]
           #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           operation program options stdout-fold initial-datum [on-error-do #false]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define args : (Listof String) (apply append options))
      (define /dev/byterr : Output-Port (open-output-bytes))
      (define stdin-silent? : Boolean (and (memq 'stdin silents) #true))
      (define stdout-silent? : Boolean (and (memq 'stdout silents) #true))
      (define stderr-silent? : Boolean (and (memq 'stderr silents) #true))

      (define subenv : (Option Environment-Variables)
        (cond [(not alt-env) #false]
              [(environment-variables? alt-env) alt-env]
              [else (alt-env)]))

      (dtrace-info #:topic operation "~a ~a" program (string-join args))

      (define-values (status datum:out)
        (with-handlers ([exn? (λ [[e : exn]] (values e initial-datum))])
          (define-values (/usr/bin/$0 /dev/outin /dev/subout /dev/errin)
            (cond [(not subenv) (apply subprocess #false #false #false program args)]
                  [else (parameterize ([current-environment-variables subenv])
                          (apply subprocess #false #false #false program args))]))

          (define ghostcat : Thread
            (thread (λ [] (with-handlers ([exn:break? void])
                            (let wait-feed-loop ([rest : (Listof Any) feeds])
                              (cond [(pair? rest)
                                     (let ([datum:in (car rest)])
                                       (cond [(eof-object? datum:in) (close-output-port /dev/subout)]
                                             [else (let ([e (sync/enable-break /dev/subout /usr/bin/$0)])
                                                     (when (eq? e /dev/subout)
                                                       (when (not stdin-silent?)
                                                         (dtrace-note (format "~a" datum:in) #:topic operation))
                                                       (displayln datum:in /dev/subout)
                                                       (flush-output /dev/subout)
                                                       (wait-feed-loop (cdr rest))))]))]
                                    [(string? feed-eof)
                                     (let ([e (sync/enable-break /dev/subout /usr/bin/$0)])
                                       (when (eq? e /dev/subout)
                                         (displayln feed-eof /dev/subout)
                                         (flush-output /dev/subout)))]
                                    [else (close-output-port /dev/subout)]))))))

          (define final-datum : a
            (let wait-fold-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin]
                                 [datum : a initial-datum])
              (define e (sync/enable-break outin-evt errin-evt /usr/bin/$0))
              
              (cond [(eq? e /dev/outin)
                     (let ([line (read-line /dev/outin)])
                       (cond [(eof-object? line) (wait-fold-loop never-evt errin-evt datum)]
                             [else (unless (not /dev/stdout) (displayln line /dev/stdout))
                                   (when (not stdout-silent?) (dtrace-note line #:topic operation #:prefix? #false))
                                   (wait-fold-loop outin-evt errin-evt (stdout-fold line datum))]))]
                    
                    [(eq? e /dev/errin)
                     (let ([line (read-line /dev/errin)])
                       (cond [(eof-object? line) (wait-fold-loop outin-evt never-evt datum)]
                             [else (unless (not /dev/stderr) (displayln line /dev/stderr))
                                   (cond [(not stderr-silent?) (dtrace-error line #:topic operation #:prefix? #false)]
                                         [else (displayln line /dev/byterr)])
                                   (wait-fold-loop outin-evt errin-evt datum)]))]
                    [else datum])))

          (thread-wait ghostcat)
          (subprocess-wait /usr/bin/$0)
          (values (subprocess-status /usr/bin/$0) final-datum)))

      (cond [(exact-integer? status)
             (unless (eq? status 0)
               (let ([maybe-errmsg (get-output-bytes /dev/byterr)])
                 (when (> (bytes-length maybe-errmsg) 0)
                   (dtrace-error (bytes->string/utf-8 maybe-errmsg) #:topic operation #:prefix? #false)))
               
               ((or on-error-do void) operation program (assert status exact-nonnegative-integer?))
               (custodian-shutdown-all (current-custodian))
               
               (raise-user-error operation "~a: exit status: ~a" (file-name-from-path program) status))]
            [(exn? status) (fg-recon-handler operation status (λ [] (custodian-shutdown-all (current-custodian))))])
      
      (custodian-shutdown-all (current-custodian))
      datum:out)))

(define fg-recon-exec : (->* (Symbol Path (Listof (Listof String)))
                             ((Option (-> Symbol Path Natural Void))
                              #:silent (Listof Exec-Silent) #:feeds (Listof Any) #:feed-eof (U EOF String)
                              #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port)
                              #:env (U False Environment-Variables (-> Environment-Variables)))
                             Void)
  (let ([stdout-void (λ [[line : String] [v : Void]] : Void v)])
    (lambda [#:silent [silents null] #:env [alt-env #false]
             #:feeds [feeds null] #:feed-eof [feed-eof eof]
             #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
             operation program options [on-error-do #false]]
      ((inst fg-recon-exec* Void)
       #:silent silents #:feeds feeds #:feed-eof feed-eof
       #:env alt-env #:/dev/stdout /dev/stdout #:/dev/stderr /dev/stderr
       operation program options stdout-void (void) on-error-do))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-mv : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (dtrace-info #:topic operation "mv ~a ~a" src dest)
    (rename-file-or-directory src dest #true)))

(define fg-recon-mv : (-> Symbol Path-String Path-String Void)
  (lambda [operation src dest]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-mv operation src dest))))

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
        (copy-port /dev/stdin /dev/stdout)))))

(define fg-recon-cat : (->* (Symbol Path-String) (Output-Port) Void)
  (lambda [operation file [/dev/stdout (current-output-port)]]
    (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler operation e))])
      (fg-cat operation file /dev/stdout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-recon-handler : (->* (Symbol exn) ((-> Void)) Void)
  (lambda [operation e [clean void]]
    (cond [(exn:recon? e) (dtrace-note #:topic operation (exn-message e))]
          [(exn:fail? e) (clean) (raise e)]
          [else #;(exn:break? e) (clean)])))
