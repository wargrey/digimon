#lang typed/racket/base

(provide (all-defined-out))

(require/typed/provide
 ffi/unsafe
 [compiler-sizeof (-> (U Symbol (Listof Symbol)) Byte)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compiler-aligned-offset : (-> (U (U Symbol (Listof Symbol)) Index) Nonnegative-Fixnum Nonnegative-Fixnum)
  (lambda [type/size offset0]
    (define type-size (if (index? type/size) type/size (compiler-sizeof type/size)))
    (define size-mod (remainder offset0 type-size))
    
    (cond [(= size-mod 0) offset0]
          [else (let ([padsize (- type-size size-mod)])
                  (cond [(> padsize 0) (assert (+ offset0 padsize) fixnum?)]
                        [else '#:deadcode offset0]))])))
