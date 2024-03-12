#lang typed/racket/base

(provide (all-defined-out) current-digimon current-digivice current-free-zone)
(provide #%info digimon-waketime digimon-uptime digimon-partner digimon-system digimon-path digivice-path)

(require racket/match)
(require racket/port)
(require racket/place)

(require "digitama/system.rkt")
(require "digitama/evt.rkt")

(require "continuation.rkt")
(require "symbol.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(void (print-boolean-long-form #true)
        
      ;;; Ignore DrRacket's convention. But don't change "compiled" since
      ;;; the compiler checks bytecodes in the core collection
      ;;; which have already been compiled into <path:compiled>.
      (use-compiled-file-paths (list (build-path "compiled"))))

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
                              (with-handlers ([exn:fail:read? (λ [[e : exn]] (exn->message e #:level 'fatal #:detail stream))])
                                (if (bytes? stream)
                                    (with-input-from-bytes stream read)
                                    (box stream))))])))))

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
