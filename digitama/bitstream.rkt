#lang typed/racket/base

(provide (all-defined-out))

(require "unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bitstream-shell stx)
  (syntax-case stx []
    [(_ id (#:-> InShell Status)
        #:with /dev/bsin lookahead
        #:ingredients [magazine mgz-payload mgz-start payload pwidth eof-width total-committed]
        #:operation [peek-bits feed-bits fire-bits])
     (syntax/loc stx
       (begin (define (reset) : Void
                (set!-values (mgz-payload mgz-start) (values 0 0))
                (set!-values (payload pwidth eof-width) (values 0 0 0)))

              (define align-bits : (-> Index)
                (lambda []
                  (define skip (unsafe-fxremainder pwidth 8))

                  (feed-bits skip)
                  (begin0 (peek-bits skip)
                          (fire-bits skip))))

              (define (final-size) : Index
                (let unwind ([bits : Index pwidth]
                             [farther : Byte lookahead]
                             [size : Index mgz-start])
                  ;;; NOTE
                  ; at most `lookahead` bytes could be unpeeked.
                  ; after this call, the rest unused bits are no longer available,
                  ; and the input port would be byte-aligned.
                  (cond [(or (< bits 8) (<= farther 0) (<= size 0)) size]
                        [else (unwind (unsafe-idx- bits 8) (- farther 1) (- size 1))])))

              (define final-commit-bits : (-> Void)
                (lambda []
                  (let ([commit-size (final-size)])
                    (read-bytes! magazine /dev/bsin 0 commit-size)
                    (set! total-committed (+ total-committed commit-size))
                    (reset) #| so that double call would not cause unexpected reading |#)))

              (define id : (-> InShell Status)
                (lambda [cmd]
                  (case cmd
                    [(align) (align-bits)]
                    [(final-commit) (final-commit-bits) total-committed]
                    [(aggregate) (+ total-committed (final-size))]
                    [else 0])))))]
    
    [(_ id (#:-> OutShell Status)
        #:with /dev/bsout
        #:ingredients [tank tank-payload payload pwidth total-sent]
        #:operation [push-bits send-bits])
     (syntax/loc stx
       (begin (define (reset) : Void
                (set!-values (payload pwidth) (values 0 0)))

              (define (align-bits) : Void
                (push-bits 0 (unsafe-idx- 8 (unsafe-fxremainder pwidth 8)) #b0))

              (define id : (-> OutShell Status)
                (lambda [cmd]
                  (case cmd
                    [(align) (align-bits)]
                    [(drop) (reset)])

                  ;;; NOTE
                  ; this intentionally works for 'aggregate, and is meaningless for other operations;
                  ; maybe in the future, other operations that return a natural result would be defined.
                  total-sent))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mask-bits : (Immutable-Vectorof Index)
  (vector-immutable #x0000
                    #x0001 #x0003 #x0007 #x000f #x001f #x003f #x007f #x00ff
                    #x01ff #x03ff #x07ff #x0fff #x1fff #x3fff #x7fff #xffff))

(define bits-mask : (-> Index Index)
  (lambda [nbits]
    (if (< nbits 17)
        (unsafe-vector*-ref mask-bits nbits)
        (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))))
