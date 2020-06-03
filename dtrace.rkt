#lang typed/racket/base

(provide (all-defined-out))

(require "continuation.rkt")
(require "symbol.rkt")
(require "format.rkt")
(require "echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dtrace-Receiver (-> Symbol String Any Symbol Any))

(define /dev/log : Logger (make-logger 'digimon (current-logger)))
(define /dev/dtrace : Logger (make-logger 'dtrace #false))

(define dtrace-blank-topic : Symbol '_)

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
      [else (echof #:fgcolor 248 "~a~n" message)])))

(define make-dtrace-loop : (->* ()
                                (Logger Log-Level
                                        #:exit-predicate? (-> Any Boolean) #:atexit (-> Any) #:silent-topics (Listof Symbol)
                                        #:false-receiver Dtrace-Receiver #:blank-receiver Dtrace-Receiver
                                        #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:else-receiver Dtrace-Receiver)
                                (-> Void))
  (lambda [[logger /dev/dtrace] [level 'debug]
                  #:exit-predicate? [exit? eof-object?] #:atexit [atexit void] #:silent-topics [silent-topics dtrace-silent-topics]
                  #:false-receiver [false-receiver void] #:blank-receiver [blank-receiver dtrace-event-echo]
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
            [(eq? topic dtrace-blank-topic) (blank-receiver level message urgent-data dtrace-blank-topic) (trace)]
            [else (let ([maybe-receiver (assq topic receivers)])
                    (cond [(and maybe-receiver) ((cdr maybe-receiver) level message urgent-data (assert topic))]
                          [(not topic) (false-receiver level message urgent-data '||)]
                          [else (else-receiver level message urgent-data topic)])
                    (trace))]))

    trace))

(define call-with-dtrace : (All (a) (->* ((-> a))
                                         (Logger Log-Level
                                                 #:silent-topics (Listof Symbol) #:false-receiver Dtrace-Receiver #:blank-receiver Dtrace-Receiver
                                                 #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:else-receiver Dtrace-Receiver)
                                         a))
  (lambda [proc [logger /dev/dtrace] [level 'debug]
                #:silent-topics [silent-topics dtrace-silent-topics]
                #:false-receiver [false-receiver dtrace-event-echo] #:blank-receiver [blank-receiver dtrace-event-echo]
                #:topic-receivers [receivers null] #:else-receiver [else-receiver dtrace-event-echo]]
    (define sentry : Symbol (gensym 'dtrace))
    (define (sentry? urgent) : Boolean (eq? urgent sentry))
    
    (define dtrace : Thread
      (thread (make-dtrace-loop logger level
                                #:exit-predicate? sentry? #:silent-topics silent-topics
                                #:false-receiver false-receiver #:blank-receiver blank-receiver
                                #:topic-receivers receivers #:else-receiver else-receiver)))

    (begin0
      (parameterize ([current-logger logger]) (proc))
      (log-message logger 'debug "Done" sentry)
      (thread-wait dtrace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-send : (->* (Any Symbol String Any) (Any) Void)
  (lambda [topic level message urgent [prefix? #false]]
    (define log-level : Log-Level (case level [(debug info warning error fatal) level] [else 'debug]))
    (cond [(logger? topic) (log-message topic log-level message urgent prefix?)]
          [(symbol? topic) (log-message (current-logger) log-level topic message urgent prefix?)]
          [else (log-message (current-logger) log-level (value-name topic) message urgent prefix?)])))

(define-values (dtrace-debug dtrace-info dtrace-warning dtrace-error dtrace-fatal)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
                  (λ [#:topic [t (current-logger)] #:urgent [u (current-continuation-marks)] #:prefix? [? #false] msgfmt . messages]
                    (dtrace-send t level (if (null? messages) msgfmt (apply format msgfmt messages)) u ?)))])
    (values (dtrace 'debug) (dtrace 'info) (dtrace 'warning) (dtrace 'error) (dtrace 'fatal))))

(define log-string : (->* (Log-Level String) (#:logger Logger #:topic (Option Symbol) #:urgent Any #:prefix? Boolean) #:rest Any Void)
  (lambda [level msgfmt #:logger [logger (current-logger)] #:topic [topic dtrace-blank-topic] #:urgent [urgent #false] #:prefix? [prefix? #true] . argl]
    (log-message logger level topic (~string msgfmt argl) urgent
                 (and prefix? (not (eq? topic dtrace-blank-topic))))))

(define log-datum : (->* (Log-Level Any) (String #:logger Logger #:topic (Option Symbol) #:prefix? Boolean) #:rest Any Void)
  (lambda [level urgent #:logger [logger (current-logger)] #:topic [topic dtrace-blank-topic] #:prefix? [prefix? #true] [msgfmt ""] . argl]
    (log-message logger level topic (~string msgfmt argl) urgent
                 (and prefix? (not (eq? topic dtrace-blank-topic))))))

(define dtrace-exception : (->* (exn) (#:logger Logger #:level Log-Level #:topic (Option Symbol) #:prefix? Boolean #:brief? Boolean) Void)
  (lambda [errobj #:logger [logger (current-logger)] #:level [level 'error] #:topic [topic dtrace-blank-topic] #:prefix? [prefix? #true] #:brief? [brief? #true]]
    (define /dev/errout : Output-Port (open-output-string))

    (unless (not prefix?)
      (display (object-name errobj) /dev/errout)
      (display #\: /dev/errout)
      (display #\space /dev/errout))

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
    
    (log-message logger level topic
                 (get-output-string /dev/errout) errobj
                 (and prefix? (not (eq? topic dtrace-blank-topic))))))
