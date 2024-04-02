#lang typed/racket/base

(provide (all-defined-out))

(require "string.rkt")
(require "../dtrace.rkt")

;;; Racket's initial logger writes error messages to stderr (and syslog)
;;; The logger facility is a thing that deep into the Racket Virtual Machine but parameters are thread specific data.
;;; The point is `current-logger` is not guaranteed to be the same one even though their names say so.
;;; If your `make-dtrace-loop` does not dispatch some messages, it probably has not received them at all.

;;; Please keep above in mind whenever you want to send messages to `current-logger`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dtrace-Level (U 'emergency 'alert 'critical 'fatal 'error 'warning 'notice 'info 'note 'debug 'trace))

(define dtrace-topic : Symbol 'dtrace)

(define /dev/log : Logger (make-logger 'digimon (current-logger)))
(define /dev/dtrace : Logger
  (let* ([maybe-dtrace-in-fresh-namespace (current-logger)])
    (cond [(eq? (logger-name maybe-dtrace-in-fresh-namespace) dtrace-topic) maybe-dtrace-in-fresh-namespace]
          [else (make-logger dtrace-topic #false)])))

(define dtrace-symbol->level : (-> Symbol Log-Level)
  (lambda [level]
    (case level
      [(debug info warning error fatal) level]
      [(note notice) 'info]
      [(emergency alert critical) 'fatal]
      [else 'debug])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-send : (->* (Any Symbol String Any) (Any) Void)
  (lambda [topic level message urgent [prefix? #true]]
    (define log-level : Log-Level (dtrace-symbol->level level))
    (define udata : Any (if (eq? log-level level) urgent (dtrace level urgent)))
    
    (cond [(logger? topic) (log-message topic log-level message udata (and prefix? (not (eq? (logger-name topic) dtrace-topic))))]
          [(symbol? topic) (log-message /dev/dtrace log-level topic message udata (and prefix? (not (eq? topic dtrace-topic))))]
          [else (log-message /dev/dtrace log-level (string->symbol (format "<~a>" topic)) message udata prefix?)])))

(define dtrace-message : (->* (Symbol String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
  (lambda [level #:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-trace dtrace-debug dtrace-note dtrace-info dtrace-notice dtrace-warning
                             dtrace-error dtrace-fatal dtrace-critical dtrace-alert dtrace-emergency)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
                  (λ [#:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'trace) (dtrace 'debug) (dtrace 'note) (dtrace 'info) (dtrace 'notice) (dtrace 'warning)
            (dtrace 'error) (dtrace 'fatal) (dtrace 'critical) (dtrace 'altert) (dtrace 'emergency))))

(define dtrace-datum : (->* (Symbol Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
  (lambda [level u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-datum-trace dtrace-datum-debug dtrace-datum-note dtrace-datum-info dtrace-datum-notice dtrace-datum-warning
                                   dtrace-datum-error dtrace-datum-fatal dtrace-datum-critical dtrace-datum-alert dtrace-datum-emergency)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
                  (λ [u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'trace) (dtrace 'debug) (dtrace 'note) (dtrace 'info) (dtrace 'notice) (dtrace 'warning)
            (dtrace 'error) (dtrace 'fatal) (dtrace 'critical) (dtrace 'altert) (dtrace 'emergency))))

(define-values (dtrace-sentry-trace dtrace-sentry-debug dtrace-sentry-note dtrace-sentry-info dtrace-sentry-notice dtrace-sentry-warning
                                    dtrace-sentry-error dtrace-sentry-fatal dtrace-sentry-critical dtrace-sentry-alert dtrace-sentry-emergency)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (Any) (#:handler Dtrace-Receiver #:topic Any #:prefix? Boolean String #:end? Boolean) #:rest Any Void)
                  (λ [u #:handler [handler void] #:topic [t /dev/dtrace] #:prefix? [? #true] #:end? [end? #true] [msgfmt ""] . messages]
                    (dtrace-send t level (~string msgfmt messages) (dtrace-sentry u handler end?) ?)))])
    (values (dtrace 'trace) (dtrace 'debug) (dtrace 'note) (dtrace 'info) (dtrace 'notice) (dtrace 'warning)
            (dtrace 'error) (dtrace 'fatal) (dtrace 'critical) (dtrace 'altert) (dtrace 'emergency))))
