#lang typed/racket/base

(provide (all-defined-out))

(require "exn.rkt")

(require "../minimal/format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-sentry-start ())
(struct spec-sentry-end ())

(struct spec-sentry
  ([topic : Symbol]
   [sender : Logger]
   [receiver : Log-Receiver])
  #:constructor-name make-spec-sentry
  #:type-name Spec-Sentry)

(define current-spec-timeout-sentry : (Parameterof (Option Spec-Sentry)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-spec-timeout-sentry : (-> Spec-Sentry)
  (lambda []
    (define topic : Symbol (gensym 'spec-timeout-sentry))
    
    (define logger (make-logger topic #false))
    (define receiver (make-log-receiver logger 'debug topic))

    (make-spec-sentry topic logger receiver)))

(define spec-sentry-close : (-> Spec-Sentry Void)
  (lambda [self]
    (void)))

(define spec-sentry-send : (-> Spec-Sentry Any Void)
  (lambda [self datum]
    (log-message (spec-sentry-sender self) 'debug (spec-sentry-topic self) "" datum)))

(define spec-timeout-evt : (-> Spec-Sentry (Evtof Any))
  (lambda [self]
    (wrap-evt (spec-sentry-receiver self)
              (λ [[v : (Immutable-Vector Symbol String Any (Option Symbol))]]
                (vector-ref v 2)))))

(define spec-timeout-start : (-> Void)
  (let ([datum (spec-sentry-start)])
    (lambda []
      (define sentry (current-spec-timeout-sentry))

      (unless (not sentry)
        (spec-sentry-send sentry datum)))))

(define spec-timeout-terminate : (-> Void)
  (let ([datum (spec-sentry-end)])
    (lambda []
      (define sentry (current-spec-timeout-sentry))

      (unless (not sentry)
        (spec-sentry-send sentry datum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-timeout-invoke : (-> Spec-Sentry Natural (-> Any) Any)
  (lambda [self timeout action]
    (define sentry-evt : (Evtof Any) (spec-timeout-evt self))
    
    (parameterize ([current-spec-timeout-sentry self])
      (define ghostcat (thread (λ [] (spec-sentry-send self (action)))))
      
      (with-handlers ([exn? (λ [[e : exn]] (kill-thread ghostcat) e)])
        (let restart ([s : Nonnegative-Real (max (/ timeout 1000.0) 0.0)])
          (define datum (sync/timeout/enable-break s sentry-evt))
          (cond [(not datum) (spec-throw "timeout (longer than ~as)" (~gctime timeout))]
                [(spec-sentry-start? datum) (restart s)]
                [(spec-sentry-end? datum) (sync/enable-break sentry-evt)]
                [else datum]))))))
