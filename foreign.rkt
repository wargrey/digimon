#lang typed/racket/base

(provide (all-defined-out))

(require/typed/provide
 ffi/unsafe
 [compiler-sizeof (-> Symbol Byte)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compiler-aligned-offset : (-> Symbol Nonnegative-Fixnum Nonnegative-Fixnum)
  (lambda [type offset0]
    (define sizeof (compiler-sizeof type))
    (define sizemod (remainder offset0 sizeof))
    
    (cond [(= sizemod 0) offset0]
          [else (let ([padsize (- sizeof sizemod)])
                  (cond [(> padsize 0) (assert (+ offset0 padsize) fixnum?)]
                        [else '#:deadcode offset0]))])))
