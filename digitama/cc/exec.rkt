#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "../../echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-toolchain-exec : (-> Symbol Path (Listof (Listof String)) Symbol Void)
  (lambda [operation program options system]
    (parameterize ([subprocess-group-enabled #true]
                   [current-subprocess-custodian-mode 'kill]
                   [current-custodian (make-custodian)])
      (define args : (Listof String) (apply append options))
      (define-values (c-toolchain /dev/outin /dev/stdout /dev/errin)
        (apply subprocess #false #false #false program args))
      
      (echof #:fgcolor 'cyan "~a: ~a ~a~n" operation program (string-join args))

      (with-handlers ([exn? (λ _ (subprocess-kill c-toolchain #true))])
        (let wait-response-loop ([outin-evt : (Rec x (Evtof x)) /dev/outin]
                                 [errin-evt : (Rec x (Evtof x)) /dev/errin])
          (define e (sync/enable-break outin-evt errin-evt c-toolchain))
     
          (cond [(eq? e /dev/outin)
                 (let ([line (read-line /dev/outin)])
                   (cond [(eof-object? line) (wait-response-loop never-evt errin-evt)]
                         [else (echof #:fgcolor 'darkgray "~a~n" line)
                               (wait-response-loop outin-evt errin-evt)]))]
                [(eq? e /dev/errin)
                 (let ([line (read-line /dev/errin)])
                   (cond [(eof-object? line) (wait-response-loop outin-evt never-evt)]
                         [else (eechof #:fgcolor 'darkred "~a~n" line)
                               (wait-response-loop outin-evt errin-evt)]))])))

      (subprocess-wait c-toolchain)
      (custodian-shutdown-all (current-custodian))

      (let ([status (subprocess-status c-toolchain)])
        (unless (eq? status 0)
          (raise-user-error 'exec "~a terminated with exit code ~a"
                            (file-name-from-path program) status))))))
