#lang typed/racket/base

(provide (all-defined-out))

(require "format.rkt")
(require "echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Log-Event-Receiver (-> Symbol String Any Symbol Any))

(define log-silent-racket-topics : (Listof Symbol)
  '(GC racket/contract optimizer place syntax-parse
       collapsible-value-bailout collapsible-contract-bailout
       module-prefetch tr-timing online-check-syntax cm-accomplice))

(define log-event-echo : Log-Event-Receiver
  (lambda [level message urgent topic]
    (case level
      [(debug) (echof #:fgcolor 248 "~a~n" message)]
      [(info) (echof #:fgcolor 'cyan "~a~n" message)]
      [(warning) (echof #:fgcolor 'yellow "~a~n" message)]
      [(error fatal) (echof #:fgcolor 'red "~a~n" message)])))

(define make-log-trace : (->* (Logger)
                              (Log-Level #:exit-predicate? (-> Any Boolean) #:atexit (-> Any) #:silent-topics (Listof Symbol)
                                         #:false-receiver Log-Event-Receiver #:blank-receiver Log-Event-Receiver
                                         #:topic-receivers (Listof (Pairof Symbol Log-Event-Receiver)) #:else-receiver Log-Event-Receiver)
                              (-> Void))
  (lambda [logger [level 'debug]
                  #:exit-predicate? [exit? eof-object?] #:atexit [atexit void] #:silent-topics [silent-topics log-silent-racket-topics]
                  #:false-receiver [false-receiver void] #:blank-receiver [blank-receiver log-event-echo]
                  #:topic-receivers [receivers null] #:else-receiver [else-receiver log-event-echo]]
    (define /dev/log : Log-Receiver (make-log-receiver logger level))
    
    (define (trace) : Void
      (define log : (Immutable-Vector Symbol String Any (Option Symbol)) (sync/enable-break /dev/log))
      (define level : Symbol (vector-ref log 0))
      (define message : String (vector-ref log 1))
      (define urgent-data : Any (vector-ref log 2))
      (define topic : (Option Symbol) (vector-ref log 3))
      
      (cond [(exit? urgent-data) (void (atexit))]
            [(memq topic silent-topics) (trace)]
            [(eq? topic '||) (blank-receiver level message urgent-data '_) (trace)]
            [else (let ([maybe-receiver (assq topic receivers)])
                    (cond [(and maybe-receiver) ((cdr maybe-receiver) level message urgent-data (assert topic))]
                          [(not topic) (false-receiver level message urgent-data '_)]
                          [else (else-receiver level message urgent-data topic)])
                    (trace))]))

    trace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define log-string : (->* (Log-Level String) (#:logger Logger #:topic (Option Symbol) #:urgent Any #:prefix? Boolean) #:rest Any Void)
  (lambda [level msgfmt #:logger [logger (current-logger)] #:topic [topic '||] #:urgent [urgent #false] #:prefix? [prefix? #true] . argl]
    (log-message logger level topic (~string msgfmt argl) urgent
                 (and prefix? (not (eq? topic '||))))))

(define log-datum : (->* (Log-Level Any) (String #:logger Logger #:topic (Option Symbol) #:prefix? Boolean) #:rest Any Void)
  (lambda [level urgent #:logger [logger (current-logger)] #:topic [topic '||] #:prefix? [prefix? #true] [msgfmt ""] . argl]
    (log-message logger level topic (~string msgfmt argl) urgent
                 (and prefix? (not (eq? topic '||))))))

(define log-exception : (->* (exn) (#:logger Logger #:level Log-Level #:topic (Option Symbol) #:prefix? Boolean) Void)
  (lambda [errobj #:logger [logger (current-logger)] #:level [level 'error] #:topic [topic '||] #:prefix? [prefix? #true]]
    (log-message logger level topic
                 (cond [(not prefix?) (exn-message errobj)]
                       [else (format "~a: ~a" (object-name errobj) (exn-message errobj))])
                 errobj
                 (and prefix? (not (eq? topic '||))))))
