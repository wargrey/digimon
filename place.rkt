#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require racket/port)
(require racket/place)

(require "digitama/system.rkt")
(require "digitama/evt.rkt")

(require "continuation.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-synced-place-channel : (Parameterof (Option Place-Channel)) (make-parameter #false))
(define place-channel-evt : (-> Place-Channel [#:hint (Parameterof (Option Place-Channel))] (Evtof Any))
  (lambda [source-evt #:hint [hint the-synced-place-channel]]
    (hint #false)
    (wrap-evt source-evt ; do not work with guard evt since the maker may not be invoked
              (λ [datum] (hint source-evt)
                (if (place-message? datum)

                    (let ([stream : Any (place-message-stream datum)])
                      (with-handlers ([exn:fail:read? (λ [[e : exn]] (exn->message e #:level 'fatal #:detail stream))])
                        (if (bytes? stream)
                            (with-input-from-bytes stream read)
                            (box stream))))

                    datum)))))

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

(define place-statistics : (-> (Values Integer Integer Integer Integer Integer Integer))
  (let ([stat : (Vectorof Integer) (vector 0 0 0 0 0 0 0 0 0 0 0 0)])
    (lambda []
      (vector-set-performance-stats! stat)
      (values (vector-ref stat 0) (vector-ref stat 1) (vector-ref stat 2)
              (vector-ref stat 3) (vector-ref stat 6)
              (+ (vector-ref stat 10) (current-memory-use))))))
