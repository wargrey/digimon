#lang typed/racket/base

(provide (all-defined-out))

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (LZ77-Select-Codeword s)
  (case-> [Bytes Index Byte s -> s]
          [Bytes Index Index Index s -> s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-compress : (All (s) (->* (Bytes (LZ77-Select-Codeword s) s) (#:hash-bits Positive-Byte #:min-match Positive-Byte Index Index) Void))
  (lambda [src codeword-select seed [start 0] [end (bytes-length src)] #:hash-bits [hash-bits 15] #:min-match [min-match 3]]
    (define min-match-1 : Byte (- min-match 1))
    (define hash-size : Index (unsafe-idxlshift 1 hash-bits))
    (define hash-shift : Byte (remainder (+ hash-bits min-match-1) min-match))
    (define hash-mask : Index (unsafe-idx- hash-size 1))
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-accumulative-hash : (-> Index Byte Byte Index Index)
  (lambda [hv b shift mask]
    (bitwise-and mask
                 (bitwise-xor (unsafe-idxlshift hv shift)
                              b))))
