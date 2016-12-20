#lang typed/racket

(provide (all-defined-out) current-digimon digimon-waketime digimon-partner digimon-system digimon-path)
(provide #%info /dev/stdin /dev/stdout /dev/stderr /dev/eof /dev/null echof eechof)
(provide Racket-Place-Status Racket-Thread-Status Info-Ref)

(require racket/fixnum)

(require typed/racket/random)

(require "digitama/system.rkt")
(require "digitama/sugar.rkt")

(define-type EvtSelf (Rec Evt (Evtof Evt)))
(define-type Place-EvtExit (Evtof (Pairof Place Integer)))
(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Fixnum Fixnum))))
(define-type Continuation-Stack (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))))

(define /dev/log : Logger (current-logger))
(define /dev/dtrace : Logger (make-logger 'digimon #false))

(define /dev/zero : Input-Port
  (make-input-port '/dev/zero
                   (λ [[bs : Bytes]]
                     (bytes-fill! bs #x00)
                     (bytes-length bs))
                   #false
                   void))

(define /dev/urandom : Input-Port
  (make-input-port '/dev/urandom
                   (λ [[bs : Bytes]]
                     (let ([bsize (bytes-length bs)])
                       (bytes-copy! bs 0 (crypto-random-bytes bsize))
                       bsize))
                   #false
                   void))

(void (print-boolean-long-form #true)
      (current-logger /dev/dtrace)
        
      ;;; Ignore DrRacket's convention. But don't change "compiled" since
      ;;; the compiler checks the bytecodes in the core collection
      ;;; which have already been compiled into <path:compiled>.
      (use-compiled-file-paths (list (build-path "compiled"))))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))

(define string-null? : (-> Any Boolean : #:+ String)
  (lambda [str]
    (and (string? str)
         (string=? str ""))))

(define value-name : (-> Any Symbol)
  (lambda [v]
    (define name (object-name v))
    (or (and (symbol? name) name)
        (and name (string->symbol (format "<object-name:~a>" name)))
        (string->symbol (format "<object-value:~a>" v)))))

(define tee : (All (a) (-> a [#:printer (-> Any Output-Port Any)] Output-Port * a))
  (lambda [v #:printer [<< pretty-print] . outs]
    (for ([out (in-list (cons (current-output-port) outs))]) (<< v out))
    v))

(define read/assert : (All (a) (-> Any (-> Any Boolean : #:+ a) [#:from-string Boolean] a))
  (lambda [src type? #:from-string [? #true]]
    (define v : Any
      (cond [(and ? (string? src)) (read (open-input-string src))]
            [(and ? (bytes? src)) (read (open-input-bytes src))]
            [(or (path? src) (path-string? src)) (call-with-input-file src read)]
            [(input-port? src) (read src)]
            [else src]))
    (cond [(type? v) v]
          [(not (eof-object? v)) (raise-result-error 'read/assert (~a (object-name type?)) v)]
          [else (raise (make-exn:fail:read:eof (format "read/assert: ~a: unexpected <eof>" (object-name type?))
                                               (current-continuation-marks)
                                               null))])))

(define current-microseconds : (-> Fixnum)
  (lambda []
    (fl->fx (real->double-flonum (* (current-inexact-milliseconds) 1000)))))

(define timer-evt : (->* (Fixnum) (Fixnum) Timer-EvtSelf)
  (lambda [interval [basetime (current-milliseconds)]]
    (define alarm-time : Fixnum (fx+ basetime interval))
    ((inst wrap-evt Any (Vector Timer-EvtSelf Fixnum Fixnum))
     (alarm-evt alarm-time)
     (λ [alarm] (vector (timer-evt interval alarm-time) interval alarm-time)))))

(define timer-thread : (-> Fixnum (-> Thread Fixnum Any) [#:basetime Fixnum] Thread)
  (lambda [interval on-timer #:basetime [basetime (current-milliseconds)]]
    (define thdsrc : Thread (current-thread))
    (thread (thunk (let wait-dotask-loop ([evt (timer-evt interval basetime)])
                     (match (sync/enable-break evt)
                       [(vector (? evt? next-alarm) (? fixnum? interval) (? fixnum? alarm-time))
                        (on-timer thdsrc (fxquotient (fx- alarm-time basetime) interval))
                        (wait-dotask-loop next-alarm)]))))))

(define continuation-mark->stacks : (->* () ((U Continuation-Mark-Set Thread)) (Listof Continuation-Stack))
  (lambda [[cm (current-continuation-marks)]]
    ((inst map (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))) (Pairof (Option Symbol) Any))
     (λ [[stack : (Pairof (Option Symbol) Any)]]
       (define maybe-name (car stack))
       (define maybe-srcinfo (cdr stack))
       (cons (or maybe-name 'λ)
             (and (srcloc? maybe-srcinfo)
                  (let ([src (srcloc-source maybe-srcinfo)]
                        [line (srcloc-line maybe-srcinfo)]
                        [column (srcloc-column maybe-srcinfo)])
                    (vector (if (symbol? src) src (~a src))
                            (or line -1)
                            (or column -1))))))
     (cond [(continuation-mark-set? cm) (continuation-mark-set->context cm)]
           [else (continuation-mark-set->context (continuation-marks cm))]))))

(define-type Log-Message msg:log)
(struct msg:log ([level : Log-Level] [brief : String] [detail : Any] [topic : Symbol]) #:prefab #:constructor-name make-log-message)
(struct msg:exn msg:log ([stacks : (Listof Continuation-Stack)]) #:prefab #:constructor-name make-error-message)

(define dtrace-send : (-> Any Symbol String Any Void)
  (lambda [topic level message urgent]
    (define log-level : Log-Level (case level [(debug info warning error fatal) level] [else 'debug]))
    (cond [(logger? topic) (log-message topic log-level message urgent)]
          [(symbol? topic) (log-message (current-logger) log-level topic message urgent)]
          [else (log-message (current-logger) log-level (value-name topic) message urgent)])))

(define-values (dtrace-debug dtrace-info dtrace-warning dtrace-error dtrace-fatal)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any) #:rest Any Void)
                  (lambda [#:topic [topic (current-logger)] #:urgent [urgent (current-continuation-marks)] msgfmt . messages]
                    (dtrace-send topic level (if (null? messages) msgfmt (apply format msgfmt messages)) urgent)))])
    (values (dtrace 'debug) (dtrace 'info) (dtrace 'warning) (dtrace 'error) (dtrace 'fatal))))

(define dtrace-message : (-> Log-Message [#:logger Logger] [#:alter-topic (Option Symbol)] Void)
  (lambda [info #:logger [logger (current-logger)] #:alter-topic [topic #false]]
    (log-message logger
                 (msg:log-level info)
                 (or topic (msg:log-topic info))
                 (msg:log-brief info)
                 info)))

(define exn->message : (-> exn [#:level Log-Level] [#:detail Any] Log-Message)
  (lambda [e #:level [level 'error] #:detail [detail #false]]
    (make-error-message level (exn-message e) detail (value-name e)
                        (continuation-mark->stacks (exn-continuation-marks e)))))

(define the-synced-place-channel : (Parameterof (Option Place-Channel)) (make-parameter #false))
(define place-channel-evt : (-> Place-Channel [#:hint (Parameterof (Option Place-Channel))] (Evtof Any))
  (lambda [source-evt #:hint [hint the-synced-place-channel]]
    (hint #false)
    (wrap-evt source-evt ; do not work with guard evt since the maker may not be invoked
              (λ [datum] (hint source-evt)
                (cond [(not (place-message? datum)) datum]
                      [else (let ([stream : Any (place-message-stream datum)])
                              (match/handlers (if (bytes? stream) (with-input-from-bytes stream read) (box stream))
                                [(? exn:fail:read? e) (exn->message e #:level 'fatal #:detail stream)]))])))))

(define place-channel-send : (-> Place-Channel Any Void)
  (lambda [dest datum]
    (match datum
      [(? place-message-allowed?) (place-channel-put dest datum)]
      [(? exn?) (place-channel-put dest (exn->message datum))]
      [(box (and (not (? bytes? v)) (? place-message-allowed? v))) (place-channel-put dest (place-message v))]
      [_ (place-channel-put dest (place-message (with-output-to-bytes (thunk (write datum)))))])))

(define place-channel-recv : (-> Place-Channel [#:timeout Nonnegative-Real] [#:hint (Parameterof (Option Place-Channel))] Any)
  (lambda [channel #:timeout [s +inf.0] #:hint [hint the-synced-place-channel]]
    ; Note: the `hint` can also be used to determine whether it is timeout or receiving #false
    (sync/timeout/enable-break s (place-channel-evt channel #:hint hint))))

(define place-channel-send/recv : (-> Place-Channel Any [#:timeout Nonnegative-Real] [#:hint (Parameterof (Option Place-Channel))] Any)
  (lambda [channel datum #:timeout [s +inf.0] #:hint [hint the-synced-place-channel]]
    (place-channel-send channel datum)
    (place-channel-recv channel #:timeout s #:hint hint)))

(define place-status : (-> Place (U 'running Integer))
  (lambda [p]
    (match (sync/timeout 0 (place-dead-evt p))
      [(? false?) 'running]
      [_ (place-wait p)])))

(define place-wait-evt : (-> Place Place-EvtExit)
  (lambda [p]
    (wrap-evt (place-dead-evt p)
              (λ _ (cons p (place-wait p))))))

(define thread-mailbox-evt : (-> (Evtof Any))
  (lambda []
    (wrap-evt (thread-receive-evt)
              (λ _ (thread-receive)))))

(define vector-set-place-statistics! : (-> Racket-Place-Status Void)
  (lambda [stat]
    (vector-set-performance-stats! stat)
    (vector-set! stat 10 (+ (vector-ref stat 10) (current-memory-use)))))

(define vector-set-thread-statistics! : (-> Racket-Thread-Status Thread Void)
  (lambda [stat thd]
    (vector-set-performance-stats! stat thd)))

(define make-peek-port : (->* (Input-Port) ((Boxof Natural) Symbol) Input-Port)
  (lambda [/dev/srcin [iobox ((inst box Natural) 0)] [name '/dev/tmpeek]]
    (make-input-port name
                     (λ [[s : Bytes]] : (U EOF Exact-Positive-Integer)
                       (define peeked : Natural (unbox iobox))
                       (define r (peek-bytes! s peeked /dev/srcin))
                       (set-box! iobox (+ peeked (if (number? r) r 1))) r)
                     #false
                     void)))

(define call-as-normal-termination : (-> (-> Any) [#:atinit (-> Any)] [#:atexit (-> Any)] Void)
  (lambda [#:atinit [atinit/0 void] main/0 #:atexit [atexit/0 void]]
    (define exit-racket : (-> Any AnyValues) (exit-handler))
    (define codes : (HashTable Symbol Byte) #hasheq((FATAL . 95) (ECONFIG . 96) (ENOSERVICE . 99) (EPERM . 100)))
      
    (define (terminate [status : Any]) : Any
      (parameterize ([exit-handler exit-racket])
        (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
              [(hash-ref codes status (thunk #false)) => exit]
              [else (exit 0)])))
    
    (parameterize ([exit-handler terminate])
      (exit (with-handlers ([exn? (λ [[e : exn]] (and (eprintf "~a~n" (exn-message e)) 'FATAL))]
                            [void (λ [e] (and (eprintf "(uncaught-exception-handler) => ~a~n" e) 'FATAL))])
              (dynamic-wind (thunk (with-handlers ([exn? (λ [[e : exn]] (atexit/0) (raise e))]) (atinit/0)))
                            (thunk (main/0))
                            (thunk (atexit/0))))))))

(define immutable-guard : (-> Symbol (Any -> Nothing))
  (lambda [pname]
    (λ [pval] (error pname "Immutable Parameter: ~a" pval))))

(define car.eval : (->* (Any) (Namespace) Any)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (thunk (eval sexp ns))
                      (λ result (car result)))))

(define void.eval : (->* (Any) (Namespace) Void)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (thunk (eval sexp ns)) void)))
