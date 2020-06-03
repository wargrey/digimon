#lang typed/racket/base

(provide (all-defined-out))

(require "continuation.rkt")
(require "symbol.rkt")
(require "format.rkt")
(require "echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dtrace-Receiver (-> Symbol String Any Symbol Any))

(define dtrace-topic : Symbol 'dtrace)

(define /dev/log : Logger (make-logger 'digimon (current-logger)))
(define /dev/dtrace : Logger (make-logger dtrace-topic #false))

(define dtrace-silent-topics : (Listof Symbol)
  '(GC racket/contract optimizer place syntax-parse
       collapsible-value-bailout collapsible-contract-bailout
       module-prefetch tr-timing online-check-syntax cm-accomplice))

(define dtrace-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (case level
      [(info) (echof #:fgcolor 'cyan "~a~n" message)]
      [(warning) (echof #:fgcolor 'yellow "~a~n" message)]
      [(error fatal) (echof #:fgcolor 'red "~a~n" message)]
      [else (displayln message)])))

(define make-dtrace-loop : (->* ()
                                (Logger Log-Level
                                        #:exit-predicate? (-> Any Boolean) #:atexit (-> Any) #:silent-topics (Listof Symbol)
                                        #:false-receiver Dtrace-Receiver #:dtrace-receiver Dtrace-Receiver
                                        #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:else-receiver Dtrace-Receiver)
                                (-> Void))
  (lambda [[logger /dev/dtrace] [level 'debug]
                  #:exit-predicate? [exit? eof-object?] #:atexit [atexit void] #:silent-topics [silent-topics dtrace-silent-topics]
                  #:false-receiver [false-receiver void] #:dtrace-receiver [dtrace-receiver dtrace-event-echo]
                  #:topic-receivers [receivers null] #:else-receiver [else-receiver dtrace-event-echo]]
    (define /dev/log : Log-Receiver (make-log-receiver logger level))
    
    (define (trace) : Void
      (define log : (Immutable-Vector Symbol String Any (Option Symbol)) (sync/enable-break /dev/log))
      
      (define level : Symbol (vector-ref log 0))
      (define message : String (vector-ref log 1))
      (define urgent-data : Any (vector-ref log 2))
      (define topic : (Option Symbol) (vector-ref log 3))

      (cond [(exit? urgent-data) (void (atexit))]
            [(memq topic silent-topics) (trace)]
            [(eq? topic dtrace-topic) (dtrace-receiver level message urgent-data dtrace-topic) (trace)]
            [else (let ([maybe-receiver (assq topic receivers)])
                    (cond [(and maybe-receiver) ((cdr maybe-receiver) level message urgent-data (assert topic))]
                          [(not topic) (false-receiver level message urgent-data '_)]
                          [else (else-receiver level message urgent-data topic)])
                    (trace))]))

    trace))

(define call-with-dtrace : (All (a) (->* ((-> a))
                                         (Logger Log-Level
                                                 #:silent-topics (Listof Symbol) #:false-receiver Dtrace-Receiver #:dtrace-receiver Dtrace-Receiver
                                                 #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:else-receiver Dtrace-Receiver)
                                         a))
  (lambda [proc [logger /dev/dtrace] [level 'debug]
                #:silent-topics [silent-topics dtrace-silent-topics]
                #:false-receiver [false-receiver dtrace-event-echo] #:dtrace-receiver [dtrace-receiver dtrace-event-echo]
                #:topic-receivers [receivers null] #:else-receiver [else-receiver dtrace-event-echo]]
    (define sentry : Symbol (gensym 'dtrace))
    (define (sentry? urgent) : Boolean (eq? urgent sentry))
    
    (define dtrace : Thread
      (thread (make-dtrace-loop logger level
                                #:exit-predicate? sentry? #:silent-topics silent-topics
                                #:false-receiver false-receiver #:dtrace-receiver dtrace-receiver
                                #:topic-receivers receivers #:else-receiver else-receiver)))

    (begin0
      (parameterize ([current-logger logger]) (proc))
      (log-message logger 'debug "Done" sentry)
      (thread-wait dtrace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-send : (->* (Any Symbol String Any) (Any) Void)
  (lambda [topic level message urgent [prefix? #true]]
    (define log-level : Log-Level (case level [(debug info warning error fatal) level] [else 'debug]))
    (cond [(logger? topic) (log-message topic log-level message urgent (and prefix? (not (eq? (logger-name topic) dtrace-topic))))]
          [(symbol? topic) (log-message (current-logger) log-level topic message urgent (and prefix? (not (eq? topic dtrace-topic))))]
          [else (log-message (current-logger) log-level (value-name topic) message urgent prefix?)])))

(define dtrace-message : (->* (Log-Level String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
  (lambda [level #:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-debug dtrace-info dtrace-warning dtrace-error dtrace-fatal)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
                  (λ [#:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'debug) (dtrace 'info) (dtrace 'warning) (dtrace 'error) (dtrace 'fatal))))

(define dtrace-datum : (->* (Log-Level Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
  (lambda [level u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-datum-debug dtrace-datum-info dtrace-datum-warning dtrace-datum-error dtrace-datum-fatal)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
                  (λ [u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'debug) (dtrace 'info) (dtrace 'warning) (dtrace 'error) (dtrace 'fatal))))

(define dtrace-exception : (->* (exn) (#:topic Any #:level Log-Level #:prefix? Boolean #:brief? Boolean) Void)
  (lambda [errobj #:topic [topic /dev/dtrace] #:level [level 'error] #:prefix? [prefix? #true] #:brief? [brief? #true]]
    (define /dev/errout : Output-Port (open-output-string))

    (display (object-name errobj) /dev/errout)
    (display #\: /dev/errout)
    (display #\space /dev/errout)
    (display (exn-message errobj) /dev/errout)

    (when (not brief?)
      (for ([stack (in-list (continuation-mark->stacks errobj))])
        (define maybe-location (cdr stack))
        (unless (not maybe-location)
          (display "\n»»»» " /dev/errout)
          (display (car stack) /dev/errout)
          (display #\: /dev/errout)
          (display #\space /dev/errout)
          (display (vector-ref maybe-location 0) /dev/errout)
          (display #\: /dev/errout)
          (display (vector-ref maybe-location 1) /dev/errout)
          (display #\: /dev/errout)
          (display (vector-ref maybe-location 2) /dev/errout))))
    
    (dtrace-send topic level (get-output-string /dev/errout) errobj prefix?)))
