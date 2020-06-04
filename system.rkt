#lang typed/racket/base

(provide (all-defined-out) current-digimon current-digivice current-free-zone)
(provide #%info digimon-waketime digimon-uptime digimon-partner digimon-system digimon-path digivice-path)

(require racket/fixnum)
(require racket/format)
(require racket/match)
(require racket/port)
(require racket/place)

(require "digitama/system.rkt")
(require "digitama/sugar.rkt")

(require "continuation.rkt")
(require "symbol.rkt")
(require "dtrace.rkt")

(define-type EvtSelf (Rec Evt (Evtof Evt)))
(define-type Place-EvtExit (Evtof (Pairof Place Integer)))
(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Fixnum Fixnum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(void (print-boolean-long-form #true)
      (current-logger /dev/dtrace)
        
      ;;; Ignore DrRacket's convention. But don't change "compiled" since
      ;;; the compiler checks bytecodes in the core collection
      ;;; which have already been compiled into <path:compiled>.
      (use-compiled-file-paths (list (build-path "compiled"))))

(define string-null? : (-> Any Boolean : #:+ String)
  (lambda [str]
    (and (string? str)
         (string=? str ""))))

(define maybe? : (All (a) (-> (Option a) (-> a Boolean) Boolean))
  (lambda [val ?]
    (or (not val)
        (and val (? val)))))

(define read:+? : (All (a) (-> Any (-> Any Boolean : #:+ a) [#:from-string Boolean] a))
  (lambda [src type? #:from-string [? #true]]
    (define v : Any
      (cond [(and ? (string? src)) (read (open-input-string src))]
            [(and ? (bytes? src)) (read (open-input-bytes src))]
            [(or (path? src) (path-string? src)) (call-with-input-file src read)]
            [(input-port? src) (read src)]
            [else src]))
    (cond [(type? v) v]
          [(not (eof-object? v)) (raise-result-error 'read:+? (~a (object-name type?)) v)]
          [else (raise (make-exn:fail:read:eof (format "read:+?: ~a: unexpected <eof>" (object-name type?))
                                               (current-continuation-marks)
                                               null))])))

(define hash-ref:+? : (All (a b) (->* (HashTableTop Any (-> Any Boolean : #:+ a)) ((-> b)) (U a b)))
  (lambda [src key type? [defval #false]]
    (define v : Any (if defval (hash-ref src key defval) (hash-ref src key)))
    (cond [(type? v) v]
          [(not defval) (raise-result-error 'hash-ref:+? (~a (object-name type?)) v)]
          [else (defval)])))

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
    (thread (λ [] (let wait-dotask-loop ([evt (timer-evt interval basetime)])
                    (match (sync/enable-break evt)
                      [(vector (? evt? next-alarm) (? fixnum? interval) (? fixnum? alarm-time))
                       (on-timer thdsrc (fxquotient (fx- alarm-time basetime) interval))
                       (wait-dotask-loop next-alarm)]))))))

(define exn->message : (-> exn [#:level Log-Level] [#:detail Any] (Vector Log-Level String Any Symbol (Listof Continuation-Stack)))
  (lambda [e #:level [level 'error] #:detail [detail #false]]
    (vector level (exn-message e) detail (datum-name e)
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
      [_ (place-channel-put dest (place-message (with-output-to-bytes (λ [] (write datum)))))])))

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
    (if (sync/timeout 0 (place-dead-evt p)) (place-wait p) 'running)))

(define place-wait-evt : (-> Place Place-EvtExit)
  (lambda [p]
    (wrap-evt (place-dead-evt p)
              (λ _ (cons p (place-wait p))))))

(define thread-mailbox-evt : (-> (Evtof Any))
  (lambda []
    (wrap-evt (thread-receive-evt)
              (λ _ (thread-receive)))))

(define place-statistics : (-> (Values Integer Integer Integer Integer Integer Integer))
  (let ([stat : (Vectorof Integer) (vector 0 0 0 0 0 0 0 0 0 0 0 0)])
    (lambda []
      (vector-set-performance-stats! stat)
      (values (vector-ref stat 0) (vector-ref stat 1) (vector-ref stat 2)
              (vector-ref stat 3) (vector-ref stat 6)
              (+ (vector-ref stat 10) (current-memory-use))))))

(define thread-statistics : (->* () (Thread) (Values Boolean Boolean Boolean Natural))
  (let ([stat : (Vectorof (U Boolean Integer)) (vector #false #false #false 0)])
    (lambda [[thd (current-thread)]]
      (vector-set-performance-stats! stat thd)
      (values (and (vector-ref stat 0) #true)
              (and (vector-ref stat 1) #true)
              (and (vector-ref stat 2) #true)
              (let ([bs : (U Boolean Integer) (vector-ref stat 3)])
                (if (exact-nonnegative-integer? bs) bs 0))))))

(define call-as-normal-termination : (-> (-> Any) [#:atinit (-> Any)] [#:atexit (-> Any)] Void)
  (lambda [#:atinit [atinit/0 void] main/0 #:atexit [atexit/0 void]]
    (define exit-racket : (-> Any AnyValues) (exit-handler))
    (define codes : (HashTable Symbol Byte) #hasheq((FATAL . 95) (ECONFIG . 96) (ENOSERVICE . 99) (EPERM . 100)))
    
    (define (terminate [status : Any]) : Any
      (parameterize ([exit-handler exit-racket])
        (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
              [(hash-ref codes status (λ [] #false)) => exit]
              [else (exit 0)])))
    
    (parameterize ([exit-handler terminate])
      (exit (with-handlers ([exn? (λ [[e : exn]] (and (eprintf "~a~n" (exn-message e)) 'FATAL))]
                            [void (λ [e] (and (eprintf "(uncaught-exception-handler) => ~a~n" e) 'FATAL))])
              (dynamic-wind (λ [] (with-handlers ([exn? (λ [[e : exn]] (atexit/0) (raise e))]) (atinit/0)))
                            (λ [] (main/0))
                            (λ [] (atexit/0))))))))

(define immutable-guard : (-> Symbol (Any -> Nothing))
  (lambda [pname]
    (λ [pval] (error pname "Immutable Parameter: ~a" pval))))

(define car.eval : (->* (Any) (Namespace) Any)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (λ [] (eval sexp ns))
                      (λ result (car result)))))

(define void.eval : (->* (Any) (Namespace) Void)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (λ [] (eval sexp ns)) void)))
