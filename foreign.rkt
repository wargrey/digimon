#lang typed/racket/base

(provide (all-defined-out))

(require/typed/provide
 ffi/unsafe
 [compiler-sizeof (-> Symbol Byte)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compiler-aligned-offset : (-> (U Symbol Index) Nonnegative-Fixnum Nonnegative-Fixnum)
  (lambda [type/size offset0]
    (define type-size (if (symbol? type/size) (compiler-sizeof type/size) type/size))
    (define size-mod (remainder offset0 type-size))
    
    (cond [(= size-mod 0) offset0]
          [else (let ([padsize (- type-size size-mod)])
                  (cond [(> padsize 0) (assert (+ offset0 padsize) fixnum?)]
                        [else '#:deadcode offset0]))])))
