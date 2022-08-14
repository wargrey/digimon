#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [vector-ref unsafe-vector*-ref] [vector-set! unsafe-vector*-set!]
                     [bytes-ref unsafe-bytes-ref] [bytes-set! unsafe-bytes-set!] [bytes-copy! unsafe-bytes-copy!]))

(require racket/fixnum)
(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-b+ : (-> Index Byte Byte)
  (lambda [b1 b2]
    (assert (fx+ b1 b2) byte?)))

(define unsafe-b* : (-> Index Byte Byte)
  (lambda [b1 b2]
    (assert (fx* b1 b2) byte?)))

(define unsafe-brshift : (-> Index Byte Byte)
  (lambda [b rs]
    (assert (fxrshift b rs) byte?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-idx* : (-> Natural Natural Index)
  (lambda [i1 i2]
    (assert (fx* i1 i2) index?)))

(define unsafe-idx+ : (-> Natural Natural Index)
  (lambda [i1 i2]
    (assert (fx+ i1 i2) index?)))

(define unsafe-idx- : (case-> [Byte Byte -> Byte]
                              [Zero Negative-Fixnum -> Index]
                              [Natural Natural -> Index])
  (lambda [i1 i2]
    (if (and (byte? i1) (byte? i2))
        (assert (fx- i1 i2) byte?)
        (assert (fx- i1 i2) index?))))

(define unsafe-idxxor : (-> Integer Natural Index)
  (lambda [i1 i2]
    (assert (fxxor i1 i2) index?)))

(define unsafe-idxior : (-> Integer Natural Index)
  (lambda [i1 i2]
    (assert (fxior i1 i2) index?)))

(define unsafe-idxlshift : (case-> [Positive-Integer Fixnum -> Positive-Index]
                                   [Natural Fixnum -> Index])
  (lambda [i ls]
    (assert (fxlshift i ls) index?)))

(define unsafe-idxrshift : (case-> [Byte Byte -> Byte]
                                   [Natural Byte -> Index]
                                   [Natural Natural -> Index])
  (lambda [i rs]
    (if (and (byte? i) (byte? rs))
        (assert (fxrshift i rs) byte?)
        (assert (fxrshift i rs) index?))))
