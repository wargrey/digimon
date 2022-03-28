#lang typed/racket/base

(provide (all-defined-out))

(require/typed/provide
 ffi/unsafe
 [compiler-sizeof (-> Symbol Byte)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compiler-aligned-offset : (-> (U Symbol Byte) Nonnegative-Fixnum Nonnegative-Fixnum)
  (lambda [type offset0]
    (define sizeof (if (symbol? type) (compiler-sizeof type) type))
    (define sizemod (remainder offset0 sizeof))
    
    (cond [(= sizemod 0) offset0]
          [else (let ([padsize (- sizeof sizemod)])
                  (cond [(> padsize 0) (assert (+ offset0 padsize) fixnum?)]
                        [else '#:deadcode offset0]))])))
