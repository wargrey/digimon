#lang typed/racket/base

(provide (all-defined-out) Dtrace-Receiver dtrace-level<?)
(provide (all-from-out "digitama/minimal/dtrace.rkt"))
(provide (all-from-out "digitama/minimal/dtrecho.rkt"))

(require "digitama/minimal/dtrace.rkt")
(require "digitama/minimal/dtrecho.rkt")

(require "digitama/dtrace.rkt")
(require "digitama/bytes.rkt")
(require "digitama/evt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-silent-topics : (Listof Symbol)
  '(GC racket/contract optimizer TR-optimizer place syntax-parse
       collapsible-value-bailout collapsible-contract-bailout
       module-prefetch tr-timing online-check-syntax cm-accomplice
       sequence-specialization framework/colorer ffi-lib tr
       concurrentometer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dtrace-loop : (->* ()
                                (Dtrace-Level Logger 
                                              #:silent-topics (Listof Symbol) #:false-receiver Dtrace-Receiver #:dtrace-receiver Dtrace-Receiver
                                              #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:default-receiver Dtrace-Receiver)
                                (-> Void))
  (lambda [[dt-level 'note] [logger /dev/dtrace]
                            #:silent-topics [silent-topics dtrace-silent-topics]
                            #:false-receiver [false-receiver void] #:dtrace-receiver [dtrace-receiver dtrace-event-echo]
                            #:topic-receivers [receivers null] #:default-receiver [default-receiver dtrace-event-echo]]
    (define /dev/log : Log-Receiver (make-log-receiver logger (dtrace-symbol->level dt-level)))

    (define (trace-topics [recv-level : Symbol] [message : String] [urgent-data : Any] [topic : (Option Symbol)]) : Any
      (cond [(eq? topic dtrace-topic) (dtrace-receiver recv-level message urgent-data dtrace-topic)]
            [(not topic) (false-receiver recv-level message urgent-data '_)]
            [else (default-receiver recv-level message urgent-data topic)]))
    
    (define (dispatch-topics [recv-level : Symbol] [message : String] [urgent-data : Any] [topic : (Option Symbol)]) : Any
      (cond [(eq? topic dtrace-topic) (dtrace-receiver recv-level message urgent-data dtrace-topic)]
            [(not topic) (false-receiver recv-level message urgent-data '_)]
            [else (let ([maybe-receiver (assq topic receivers)])
                    (cond [(and maybe-receiver) ((cdr maybe-receiver) recv-level message urgent-data (assert topic))]
                          [else (default-receiver recv-level message urgent-data topic)]))]))
  
    (define dispatch (if (pair? receivers) dispatch-topics trace-topics))

    (define (trace) : Void
      (define log : (Immutable-Vector Symbol String Any (Option Symbol)) (sync/enable-break /dev/log))
      (define topic : (Option Symbol) (dtrace-received-message-topic log))

      (if (not (memq topic silent-topics))
          (let-values ([(recv-level message udata rkt-level) (dtrace-received-message-values log)])
            (cond [(not (dtrace-level-okay? dt-level recv-level rkt-level)) (trace)]
                  [(not (sentry? udata)) (dispatch recv-level message udata topic) (trace)]
                  [else ((sentry-handler udata) recv-level message (sentry-hint udata) (or topic '_))
                        (unless (sentry-terminate? udata) (trace))]))
          (trace)))
    
    trace))

(define call-with-dtrace : (All (a) (->* ((-> a))
                                         (Dtrace-Level Logger 
                                                       #:silent-topics (Listof Symbol) #:false-receiver Dtrace-Receiver #:dtrace-receiver Dtrace-Receiver
                                                       #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:default-receiver Dtrace-Receiver)
                                         a))
  (lambda [proc [level 'note] [logger /dev/dtrace]
                #:silent-topics [silent-topics dtrace-silent-topics] #:false-receiver [false-receiver dtrace-event-echo]
                #:dtrace-receiver [dtrace-receiver dtrace-event-echo] #:topic-receivers [receivers null] #:default-receiver [receiver dtrace-event-echo]]
    (define dtrace : Thread
      (thread (make-dtrace-loop #:false-receiver false-receiver #:dtrace-receiver dtrace-receiver
                                #:topic-receivers receivers #:default-receiver receiver
                                level logger)))

    (begin0
      (parameterize ([current-logger logger])
        (with-syntax ([exn:break? void]
                      [exn:fail? dtrace-exception])
          (proc)))
      (log-message logger (dtrace-symbol->level level) "Done" (dtrace-sentry #false void #true))
      (thread-wait dtrace))))

(define open-output-dtrace : (->* ()
                                  ((U Symbol (-> String (Values Symbol (Option String)))) Any
                                   #:special (U Symbol (-> Any (Values (Option Symbol) Any String)) False)
                                   #:line-mode Symbol #:prefix? Boolean #:fallback-char (Option Char))
                                  Output-Port)
  (lambda [[line-level 'debug] [topic /dev/dtrace] #:special [special-level #false] #:line-mode [mode 'any] #:prefix? [prefix? #false] #:fallback-char [echar #\uFFFD]]
    (define name : Symbol (string->symbol (format "<~a:~a>" (if (logger? topic) (logger-name topic) topic) mode)))
    (define /dev/bufout : Output-Port (open-output-bytes name))

    (define scan-line : (-> Bytes Index Index (Values (Option Index) Byte))
      (case mode
        [(any) unsafe-bytes-scan-any]
        [(return-linefeed) unsafe-bytes-scan-refeed]
        [(linefeed) unsafe-bytes-scan-linefeed]
        [(return) unsafe-bytes-scan-return]
        [(any-one) unsafe-bytes-scan-anyone]
        [else unsafe-bytes-scan-any]))
    
    (define (dtrace-write-line [line : String]) : Void
      (cond [(symbol? line-level) (dtrace-send topic line-level line #false prefix?)]
            [else (let-values ([(level message) (line-level line)])
                    (when (string? message)
                      (dtrace-send topic level message #false prefix?)))]))

    (define (dtrace-flush)
      (when (> (file-position /dev/bufout) 0)
        (dtrace-write-line (bytes->string/utf-8 (get-output-bytes /dev/bufout #true) echar))))

    (define (dtrace-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (with-asserts ([end index?])
        (cond [(= start end) (dtrace-flush)] ; explicitly calling `flush-port`
              [else (let write-line ([pos : Index (assert start index?)]
                                     [bufsize : Natural (file-position /dev/bufout)])
                      (define-values (brkpos offset) (scan-line bs pos end))
                      (cond [(not brkpos) (write-bytes bs /dev/bufout pos end)]
                            [else (let ([nxtpos (+ brkpos offset)])
                                    (dtrace-write-line
                                     (cond [(= bufsize 0) (bytes->string/utf-8 bs echar pos brkpos)]
                                           [else (let ([total (+ bufsize (write-bytes bs /dev/bufout pos brkpos))])
                                                   (bytes->string/utf-8 (get-output-bytes /dev/bufout #true 0 total) echar 0 total))]))
                                    (when (< nxtpos end)
                                      (write-line nxtpos 0)))]))]))

      (unless (not non-block/buffered?)
        ; do writing without block, say, calling `write-bytes-avail*`,
        ; usually implies flush, and can return #false if failed.
        (dtrace-flush))
      
      (- end start))

    (define (dtrace-write-special [datum : Any] [flush? : Boolean] [breakable? : Boolean]) : Boolean
      (and special-level
           (cond [(symbol? special-level) (dtrace-send topic special-level "#<special>" datum prefix?)]
                 [else (let-values ([(level value message) (special-level datum)])
                         (when (symbol? level)
                           (dtrace-send topic level message value prefix?)))])
           #t))

    (define (dtrace-close) : Void
      (dtrace-flush)
      (close-output-port /dev/bufout))
    
    (make-output-port name always-evt dtrace-write dtrace-close
                      (and special-level dtrace-write-special)
                      port-always-write-evt (and special-level port-always-write-special-evt)
                      #false void #false #false)))
