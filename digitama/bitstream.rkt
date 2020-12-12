#lang typed/racket/base

(provide (all-defined-out))

(require "unsafe/ops.rkt")

(require (for-syntax racket/base))

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
        #:with /dev/defout
        #:ingredients [payload pwidth]
        #:operation [push-bits flush-bits])
     (syntax/loc stx
       (begin (define (reset) : Void
                (set!-values (payload pwidth) (values 0 0)))
              
              (define (align-bits) : Void
                (push-bits 0 (unsafe-idx- 8 (unsafe-fxremainder pwidth 8)) #b0))

              (define id : (->* (OutShell) ((U Output-Port (-> Output-Port))) Status)
                (lambda [cmd [/dev/bsout /dev/defout]]
                  (case cmd
                    [(start-over) (reset)]
                    [(align) (align-bits)]
                    [(drop) (reset)])))))]))
