#lang typed/racket/base

;;; https://www.w3.org/TR/PNG/#D-CRCAppendix

(provide (all-defined-out))

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (magic c)
  (let ([c>>1 (unsafe-idxrshift c 1)])
    (cond [(not (bitwise-bit-set? c 0)) c>>1]
          [else (unsafe-idxxor #xEDB88320 c>>1)])))

(define tables : (HashTable Index (Vectorof Nonnegative-Fixnum)) (make-hasheq))

(define make-crc32-table : (-> Index (Vectorof Nonnegative-Fixnum))
  (lambda [n]
    (define table : (Vectorof Nonnegative-Fixnum) (make-vector n))
    (let update ([ulong : Nonnegative-Fixnum 0])
      (when (< ulong n)
        (unsafe-vector*-set! table ulong (magic (magic (magic (magic (magic (magic (magic (magic ulong #;0) #;1) #;2) #;3) #;4) #;5) #;6) #;7))
        (update (+ ulong 1))))
    table))

(define update-crc32 : (-> Natural Bytes Index Natural Natural Index)
  (lambda [crc0 raw n start stop]
    (define table : (Vectorof Nonnegative-Fixnum) (hash-ref! tables n (λ [] (make-crc32-table n))))
    (for/fold ([c : Index (assert crc0 index?)])
              ([b : Byte (in-bytes raw start stop)])
      (unsafe-idxxor (unsafe-vector*-ref table (unsafe-fxremainder (bitwise-xor c b) n))
                     (unsafe-idxrshift c 8)))))
