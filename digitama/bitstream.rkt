#lang typed/racket/base

(provide (all-defined-out))

(require "unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct bits-output-vector
  ([payload : Natural]
   [pwidth : Index]
   [tank : (Option Bytes)])
  #:type-name Bits-Output-Vector)

(define bits-cabinets : (HashTable Any Bits-Output-Vector) (make-hasheq #| Yes, it's an `eq?`-based hash table |#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bitstream-shell stx)
  (syntax-case stx []
    [(_ id (#:-> InShell Status)
        #:with /dev/defin
        #:ingredients [magazine mgz-payload mgz-start payload pwidth]
        #:operation [peek-bits feed-bits fire-bits])
     (syntax/loc stx
       (begin (define (reset) : Void
                (set!-values (mgz-payload mgz-start) (values 0 0))
                (set!-values (payload pwidth) (values 0 0)))
              
              (define align-bits : (-> (U Input-Port (-> Input-Port)) Index)
                (lambda [/dev/bsin]
                  (define skip (unsafe-fxremainder pwidth 8))

                  (feed-bits skip /dev/bsin)
                  (begin0 (peek-bits skip)
                          (fire-bits skip))))
              
              (define final-commit-bits : (-> (U Input-Port (-> Input-Port)) (U Natural EOF))
                (lambda [/dev/bsin]
                  (let unwind ()
                    ;;; NOTE
                    ; if no `lookahead`, no bytes could be unpeeked.
                    ; after this call, the rest unused bits are no longer available,
                    ; and the input port would be byte-aligned.
                    (when (and (>= pwidth 8) (> mgz-start 0))
                      (set! mgz-start (unsafe-idx- mgz-start 1))
                      (set! pwidth (unsafe-idx- pwidth 8))
                      (unwind)))
                  (begin0
                    (read-bytes! magazine (if (input-port? /dev/bsin) /dev/bsin (/dev/bsin)) 0 mgz-start)
                    (reset) #| so that double call would not cause unexpected reading |#)))

              (define id : (->* (InShell) ((U Input-Port (-> Input-Port))) Status)
                (lambda [cmd [/dev/bsin /dev/defin]]
                  (case cmd
                    [(start-over) (reset)]
                    [(align) (align-bits /dev/bsin)]
                    [(final-commit) (final-commit-bits /dev/bsin)])))))]
    [(_ id (#:-> OutShell Status)
        #:with /dev/bsout name
        #:ingredients [tank tank-payload payload pwidth]
        #:operation [push-bits send-bits])
     (syntax/loc stx
       (begin (define (reset [clean? : Boolean]) : Void
                (set!-values (payload pwidth) (values 0 0))

                (unless (not clean?)
                  (hash-remove! bits-cabinets name)))
              
              (define (align-bits) : Void
                (push-bits 0 (unsafe-idx- 8 (unsafe-fxremainder pwidth 8)) #b0))

              (define (save-vector) : Void
                (hash-set! bits-cabinets name
                           (bits-output-vector payload pwidth
                                               (and (> tank-payload 0)
                                                    (subbytes tank 0 tank-payload)))))
              
              (define (restore-vector) : Void
                (define ?v : (Option Bits-Output-Vector) (hash-ref bits-cabinets name (λ [] #false)))

                (when (bits-output-vector? ?v)
                  (set!-values (payload pwidth) (values (bits-output-vector-payload ?v) (bits-output-vector-pwidth ?v)))
                  (let ([?bs (bits-output-vector-tank ?v)])
                    (when (and ?bs)
                      (let ([tpsize (bytes-length ?bs)])
                        (bytes-copy! tank 0 ?bs 0 tpsize)
                        (set! tank-payload tpsize))))))

              (define id : (-> OutShell Status)
                (lambda [cmd]
                  (case cmd
                    [(start-over) (reset #true)]
                    [(align) (align-bits)]
                    [(drop) (reset #false)]
                    [(save) (save-vector)]
                    [(restore) (restore-vector)])))))]))
