#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/string)

(require "digitama/dtrace.rkt")
(require "digitama/bytes.rkt")

(require "continuation.rkt")
(require "symbol.rkt")
(require "format.rkt")
(require "echo.rkt")

;;; Racket's initial logger writes error messages to stderr (and syslog)
;;; The logger facility is a thing that deep into the Racket Virtual Machine and parameters are thread specific data.
;;; The point is `current-logger` is not guaranteed to be the same one even though their names say so.
;;; If your `make-dtrace-loop` does not dispatch some messages, it probably has not received them at all.

;;; Please keep above in mind whenever you want to send messages to `current-logger`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dtrace-Receiver (-> Symbol String Any Symbol Any))
(define-type Dtrace-Level (U 'emergency 'alert 'critical 'fatal 'error 'warning 'notice 'info 'debug 'trace))

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
      [(notice) (echof #:fgcolor 'green "~a~n" message)]
      [(warning) (echof #:fgcolor 'yellow "~a~n" message)]
      [(error) (echof #:fgcolor 'red "~a~n" message)]
      [(fatal) (echof #:fgcolor 'darkred "~a~n" message)]
      [(critical alert emergency) (echof #:bgcolor 'darkred "~a~n" message)]
      [(debug) (displayln message)]
      [else (echof #:fgcolor 'darkgray "~a~n" message)])))

(define dtrace-symbol->level : (-> Symbol Log-Level)
  (lambda [level]
    (case level
      [(debug info warning error fatal) level]
      [(notice) 'info]
      [(emergency alert critical) 'fatal]
      [else 'debug])))

(define dtrace-received-message : (-> (Immutable-Vector Symbol String Any (Option Symbol)) (Values Symbol String Any (Option Symbol) Symbol))
  (lambda [log]
    (define level : Symbol (vector-ref log 0))
    (define message : String (vector-ref log 1))
    (define udata : Any (vector-ref log 2))
    (define topic : (Option Symbol) (vector-ref log 3))

    (if (dtrace? udata)
        (values (dtrace-level udata) message (dtrace-urgent udata) topic level)
        (values level message udata topic level))))

(define make-dtrace-loop : (->* ()
                                (Logger Dtrace-Level
                                        #:exit-predicate? (-> Any Boolean) #:atexit (-> Any) #:display-exit-message? Boolean
                                        #:silent-topics (Listof Symbol) #:false-receiver Dtrace-Receiver #:dtrace-receiver Dtrace-Receiver
                                        #:topic-receivers (Listof (Pairof Symbol Dtrace-Receiver)) #:else-receiver Dtrace-Receiver)
                                (-> Void))
  (lambda [[logger /dev/dtrace] [dt-level 'debug]
                  #:exit-predicate? [exit? eof-object?] #:atexit [atexit void] #:display-exit-message? [show? #true]
                  #:silent-topics [silent-topics dtrace-silent-topics] #:false-receiver [false-receiver void]
                  #:dtrace-receiver [dtrace-receiver dtrace-event-echo] #:topic-receivers [receivers null]
                  #:else-receiver [else-receiver dtrace-event-echo]]
    (define /dev/log : Log-Receiver (make-log-receiver logger (dtrace-symbol->level dt-level)))
    
    (define (trace) : Void
      (define log : (Immutable-Vector Symbol String Any (Option Symbol)) (sync/enable-break /dev/log))

      (define-values  (rdt-level message urgent-data topic rlevel) (dtrace-received-message log))
      
      (cond [(exit? urgent-data) (when (and show? (non-empty-string? message)) (dtrace-receiver rdt-level message urgent-data dtrace-topic)) (void (atexit))]
            [(memq topic silent-topics) (trace)]
            [(not (dtrace-level-okay? dt-level rdt-level rlevel)) (trace)]
            [(eq? topic dtrace-topic) (dtrace-receiver rdt-level message urgent-data dtrace-topic) (trace)]
            [else (let ([maybe-receiver (assq topic receivers)])
                    (cond [(and maybe-receiver) ((cdr maybe-receiver) rdt-level message urgent-data (assert topic))]
                          [(not topic) (false-receiver rdt-level message urgent-data '_)]
                          [else (else-receiver rdt-level message urgent-data topic)])
                    (trace))]))

    trace))

(define call-with-dtrace : (All (a) (->* ((-> a))
                                         (Logger Dtrace-Level
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
                                #:exit-predicate? sentry? #:display-exit-message? #false #:silent-topics silent-topics
                                #:false-receiver false-receiver #:dtrace-receiver dtrace-receiver
                                #:topic-receivers receivers #:else-receiver else-receiver)))

    (begin0
      (parameterize ([current-logger logger]) (proc))
      (log-message logger 'debug "Done" sentry)
      (thread-wait dtrace))))

(define open-output-dtrace : (->* ()
                                  ((U Dtrace-Level (-> String (Values Log-Level (Option String))))
                                   #:special (U Log-Level (-> Any (Values (Option Log-Level) Any String)) False)
                                   #:line-mode Symbol #:prefix? Boolean #:fallback-char (Option Char))
                                  Output-Port)
  (lambda [[line-level 'debug] [topic /dev/dtrace] #:special [special-level #false] #:line-mode [mode 'any] #:prefix? [prefix? #false] #:fallback-char [echar #\uFFFD]]
    (define name : Symbol (string->symbol (format "<~a:~a>" (datum-name topic) mode)))
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

    (define (dtrace-write [bs : Bytes] [start : Natural] [end : Natural] [flush? : Boolean] [enable-break? : Boolean]) : Integer
      (with-asserts ([end index?])
        (cond [(and (= start end) (not flush?)) (dtrace-flush)]
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
                      (λ [[bytes : Bytes] [start : Natural] [end : Natural]] (wrap-evt always-evt (λ [x] (- end start))))
                      (and special-level (λ [[special : Any]] always-evt))
                      #false void #false #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-send : (->* (Any Symbol String Any) (Any) Void)
  (lambda [topic level message urgent [prefix? #true]]
    (define log-level : Log-Level (dtrace-symbol->level level))
    (define urgent-data : Any (if (eq? log-level level) urgent (dtrace level urgent)))
    (cond [(logger? topic) (log-message topic log-level message urgent-data (and prefix? (not (eq? (logger-name topic) dtrace-topic))))]
          [(symbol? topic) (log-message /dev/dtrace log-level topic message urgent-data (and prefix? (not (eq? topic dtrace-topic))))]
          [else (log-message /dev/dtrace log-level (datum-name topic) message urgent-data prefix?)])))

(define dtrace-message : (->* (Symbol String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
  (lambda [level #:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-trace dtrace-debug dtrace-info dtrace-notice dtrace-warning dtrace-error
                             dtrace-fatal dtrace-critical dtrace-alert dtrace-emergency)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any #:prefix? Boolean) #:rest Any Void)
                  (λ [#:topic [t /dev/dtrace] #:urgent [u (current-continuation-marks)] #:prefix? [? #true] msgfmt . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'trace) (dtrace 'debug) (dtrace 'info) (dtrace 'notice) (dtrace 'warning) (dtrace 'error)
            (dtrace 'fatal) (dtrace 'critical) (dtrace 'altert) (dtrace 'emergency))))

(define dtrace-datum : (->* (Log-Level Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
  (lambda [level u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
    (dtrace-send t level (~string msgfmt messages) u ?)))

(define-values (dtrace-datum-trace dtrace-datum-debug dtrace-datum-info dtrace-datum-notice dtrace-datum-warning dtrace-datum-error
                                   dtrace-datum-fatal dtrace-datum-critical dtrace-datum-alert dtrace-datum-emergency)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (Any) (#:topic Any #:prefix? Boolean String) #:rest Any Void)
                  (λ [u #:topic [t /dev/dtrace] #:prefix? [? #true] [msgfmt ""] . messages]
                    (dtrace-send t level (~string msgfmt messages) u ?)))])
    (values (dtrace 'trace) (dtrace 'debug) (dtrace 'info) (dtrace 'notice) (dtrace 'warning) (dtrace 'error)
            (dtrace 'fatal) (dtrace 'critical) (dtrace 'altert) (dtrace 'emergency))))

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
