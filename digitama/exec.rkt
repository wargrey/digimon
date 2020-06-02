#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "../logger.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Exec-Silent (U 'stdout 'stderr 'both 'none))

(define fg-exec : (->* (Symbol Path (Listof (Listof String)) Symbol)
                       ((Option (-> Symbol Path Natural Void)) #:silent Exec-Silent #:logger Logger)
                       Void)
  (lambda [operation program options system [on-error-do #false] #:silent [silent 'none] #:logger [logger (current-logger)]]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define args : (Listof String) (apply append options))
      (define /dev/byterr : Output-Port (open-output-bytes))
      (define stdout-silent? : Boolean (or (eq? silent 'stdout) (eq? silent 'both)))
      (define stderr-silent? : Boolean (or (eq? silent 'stderr) (eq? silent 'both)))
      (define-values (/usr/bin/$0 /dev/outin /dev/stdout /dev/errin)
        (apply subprocess #false #false #false program args))
      
      (log-string 'info #:logger logger #:topic operation "~a ~a" program (string-join args))

      (with-handlers ([exn? (λ _ (subprocess-kill /usr/bin/$0 #true))])
        (let wait-response-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin])
          (define e (sync/enable-break outin-evt errin-evt /usr/bin/$0))
     
          (cond [(eq? e /dev/outin)
                 (let ([line (read-line /dev/outin)])
                   (cond [(eof-object? line) (wait-response-loop never-evt errin-evt)]
                         [else (when (not stdout-silent?)
                                 (log-string 'debug line #:logger logger #:topic operation))
                               (wait-response-loop outin-evt errin-evt)]))]
                
                [(eq? e /dev/errin)
                 (let ([line (read-line /dev/errin)])
                   (cond [(eof-object? line) (wait-response-loop outin-evt never-evt)]
                         [else (cond [(not stderr-silent?) (log-string 'error line #:logger logger #:topic operation)]
                                     [else (displayln line /dev/byterr)])
                               (wait-response-loop outin-evt errin-evt)]))])))

      (subprocess-wait /usr/bin/$0)

      (let ([status (subprocess-status /usr/bin/$0)])
        (unless (eq? status 0)
          (let ([maybe-errmsg (get-output-bytes /dev/byterr)])
            (when (> (bytes-length maybe-errmsg) 0)
              (log-string 'error (bytes->string/utf-8 maybe-errmsg) #:logger logger #:topic operation)))

          ((or on-error-do void) operation program (assert status exact-nonnegative-integer?))
          (custodian-shutdown-all (current-custodian))

          (raise-user-error operation "~a: exit status: ~a"
                            (file-name-from-path program) status)))
      
      (custodian-shutdown-all (current-custodian)))))
