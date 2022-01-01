#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/port)

(require "../dtrace.rkt")
(require "../filesystem.rkt")
(require "../format.rkt")
(require "../thread.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:recon exn () #:constructor-name make-exn:recon)

(define exec-abort : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:recon (~string msgfmt argl)
                           (current-continuation-marks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Exec-Silent (U 'stdout 'stderr 'both 'none))

(define fg-recon-exec : (->* (Symbol Path (Listof (Listof String)) Symbol)
                             ((Option (-> Symbol Path Natural Void))
                              #:dtrace-silent Exec-Silent #:feeds (Listof Any)
                              #:/dev/stdout (Option Output-Port) #:/dev/stderr (Option Output-Port))
                             Void)
  (lambda [#:dtrace-silent [silent 'none] #:feeds [feeds null] #:/dev/stdout [/dev/stdout #false] #:/dev/stderr [/dev/stderr #false]
           operation program options system [on-error-do #false]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define args : (Listof String) (apply append options))
      (define /dev/byterr : Output-Port (open-output-bytes))
      (define stdout-silent? : Boolean (or (eq? silent 'stdout) (eq? silent 'both)))
      (define stderr-silent? : Boolean (or (eq? silent 'stderr) (eq? silent 'both)))

      (dtrace-info #:topic operation "~a ~a" program (string-join args))

      (define status : (U 'running Natural exn)
        (with-handlers ([exn? (λ [[e : exn]] e)])
          (define-values (/usr/bin/$0 /dev/outin /dev/subout /dev/errin)
            (apply subprocess #false #false #false program args))

          (define ghostcat : Thread
            (thread (λ [] (let wait-feed-loop ([rest : (Listof Any) feeds])
                            (when (pair? rest)
                              (define datum (car rest))
                              (cond [(eof-object? datum) (close-output-port /dev/subout)]
                                    [else (sync/enable-break /dev/subout)
                                          (displayln datum /dev/subout)
                                          (flush-output /dev/subout)
                                          (wait-feed-loop (cdr rest))]))))))
          
          (let wait-dtrace-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin])
            (define e (sync/enable-break outin-evt errin-evt /usr/bin/$0))
            
            (cond [(eq? e /dev/outin)
                   (let ([line (read-line /dev/outin)])
                     (cond [(eof-object? line) (wait-dtrace-loop never-evt errin-evt)]
                           [else (unless (not /dev/stdout) (displayln line /dev/stdout))
                                 (when (not stdout-silent?) (dtrace-note line))
                                 (wait-dtrace-loop outin-evt errin-evt)]))]
                  
                  [(eq? e /dev/errin)
                   (let ([line (read-line /dev/errin)])
                     (cond [(eof-object? line) (wait-dtrace-loop outin-evt never-evt)]
                           [else (unless (not /dev/stderr) (displayln line /dev/stderr))
                                 (cond [(not stderr-silent?) (dtrace-error line #:topic operation #:prefix? #false)]
                                       [else (displayln line /dev/byterr)])
                                 (wait-dtrace-loop outin-evt errin-evt)]))]))
          
          (subprocess-wait /usr/bin/$0)
          (thread-safe-kill ghostcat)
          (subprocess-status /usr/bin/$0)))

      (cond [(exact-integer? status)
             (unless (eq? status 0)
               (let ([maybe-errmsg (get-output-bytes /dev/byterr)])
                 (when (> (bytes-length maybe-errmsg) 0)
                   (dtrace-error (bytes->string/utf-8 maybe-errmsg) #:topic operation #:prefix? #false)))
               
               ((or on-error-do void) operation program (assert status exact-nonnegative-integer?))
               (custodian-shutdown-all (current-custodian))
               
               (raise-user-error operation "~a: exit status: ~a" (file-name-from-path program) status))]
            [(exn? status) (fg-recon-handler operation status (λ [] (custodian-shutdown-all (current-custodian))))])
      
      (custodian-shutdown-all (current-custodian)))))

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
