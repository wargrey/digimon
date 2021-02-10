#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define (byte-reverse b)
    (let reverse ([idx 0]
                  [res 0])
      (cond [(= idx 8) res]
            [(= (bitwise-and b (arithmetic-shift 1 idx)) 0) (reverse (add1 idx) res)]
            [else (reverse (add1 idx) (bitwise-ior res (arithmetic-shift 1 (- 8 1 idx))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (bits-reversed-uint8-table stx)
  (syntax-case stx []
    [(_ )
     (with-syntax ([(r-bs ...) (for/list ([idx (in-range 256)]) (byte-reverse idx))])
       (syntax/loc stx
         ((inst vector-immutable Byte) r-bs ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bits-reversed-bytes : (Immutable-Vectorof Byte) (bits-reversed-uint8-table))
