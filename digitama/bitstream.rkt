#lang typed/racket/base

(provide (all-defined-out))
(provide unsafe-bytes-ref unsafe-bytes-set!)
(provide unsafe-fxand)

(require racket/unsafe/ops)
(require typed/racket/unsafe)

(unsafe-require/typed/provide
 racket/unsafe/ops
 [unsafe-fx+ (-> Index Nonnegative-Fixnum Index)]
 [unsafe-fx- (-> Index Index Index)]
 [unsafe-fxlshift (-> Byte Fixnum Index)]
 [unsafe-fxrshift (-> Index Byte Index)])

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bitstream-shell stx)
  (syntax-case stx []
    [(_ id (#:-> Shell Status)
        #:ingredients [magazine mgz-payload mgz-start payload pwidth]
        #:operation [peek-bits feed-bits fire-bits])
     (syntax/loc stx
       (begin (define (reset) : Void
                (set! mgz-payload 0)
                (set! mgz-start 0)
                (set! payload 0)
                (set! pwidth 0))
              
              (define (align-bits) : Index
                (define skip (unsafe-fx- 8 (unsafe-fxremainder pwidth 8)))
                (cond [(or (<= skip 0) (>= skip 8)) 0]
                      [else (feed-bits skip)
                            (begin0 (peek-bits skip)
                                    (fire-bits skip))]))
              
              (define (final-commit-bits) : (U Natural EOF)
                (let unpeek ()
                  ;;; NOTE
                  ; if no `lookahead`, no bytes could be unpeeked.
                  ; after this call, the rest unused bits are no longer available,
                  ; and the input port would be byte-aligned.
                  (when (and (>= pwidth 8) (> mgz-start 0))
                    (set! mgz-start (unsafe-fx- mgz-start 1))
                    (set! pwidth (unsafe-fx- pwidth 8))
                    (unpeek)))
                (begin0
                  (read-bytes! magazine (current-input-port) 0 mgz-start)
                  (reset) #| so that double call would not cause unexpected reading |#))

              (define (id [cmd : Shell]) : Status
                (case cmd
                  [(start-over) (reset)]
                  [(align) (align-bits)]
                  [(final-commit) (final-commit-bits)]))))]))
