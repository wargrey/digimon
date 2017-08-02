#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/future)
(require racket/tcp)

(require "system.rkt")

(define tcp-server : (-> Index (Input-Port Output-Port Index -> Any)
                         [#:max-allow-wait Natural] [#:localhost (Option String)]
                         [#:timeout (Option Fixnum)] [#:on-error (exn -> Any)] [#:custodian Custodian]
                         (Values (-> Void) Index))
  (lambda [port-hit on-connection #:max-allow-wait [maxwait (processor-count)] #:localhost [ip #false]
                    #:timeout [timeout #false] #:on-error [on-error void]
                    #:custodian [server-custodian (make-custodian)]]
    (parameterize ([current-custodian server-custodian])
      (define /dev/tcp : TCP-Listener (tcp-listen port-hit maxwait #true ip))
      (define-values (localhost portno remote rport) (tcp-addresses /dev/tcp #true))
      (define saved-params-incaseof-transferring-continuation : Parameterization (current-parameterization))
      (define (wait-accept-handle-loop) : Void
        (parameterize ([current-custodian (make-custodian server-custodian)])
          (define close-session : (-> Void) (λ [] (custodian-shutdown-all (current-custodian))))
          (with-handlers ([exn:fail:network? (λ [[e : exn]] (on-error e) (close-session))])
            (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break /dev/tcp))
            (thread (λ [] ((inst dynamic-wind Any)
                           (λ [] (unless (not timeout)
                                   (timer-thread timeout (λ [server times] ; give the task a chance to live longer
                                                           (if (fx= times 1) (break-thread server) (close-session))))))
                           (λ [] ((inst call-with-parameterization Any)
                                  saved-params-incaseof-transferring-continuation
                                  (λ [] (parameterize ([current-custodian (make-custodian)])
                                          (with-handlers ([exn? (λ [[e : exn]] (on-error e))])
                                            (on-connection /dev/tcpin /dev/tcpout portno))))))
                           (λ [] (close-session)))))))
        (wait-accept-handle-loop))
      (thread wait-accept-handle-loop)
      (values (λ [] (custodian-shutdown-all server-custodian)) portno))))
