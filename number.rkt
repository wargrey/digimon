#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define integer->network-bytes : (->* (Integer) (Index Bytes Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset 0]]
    (define bsize : Nonnegative-Fixnum (max bsize0 (integer-bytes-length mpint)))
    (define bmpint : Bytes (or bmpint0 (make-bytes bsize #x00)))
    
    (let integer->octets ([sth : Nonnegative-Fixnum (assert (+ bsize offset) index?)]
                          [mpint : Integer mpint])
      (define sth-8 : Fixnum (- sth 8))
      (define sth-4 : Fixnum (- sth 4))
      (define sth-1 : Fixnum (- sth 1))
      
      (cond [(>= sth-8 offset)
             (integer->integer-bytes (bitwise-and mpint #xFFFFFFFFFFFFFFFF) 8 #false #true bmpint sth-8)
             (integer->octets sth-8 (arithmetic-shift mpint -64))]
            [(>= sth-4 offset)
             (integer->integer-bytes (bitwise-and mpint #xFFFFFFFF) 4 #false #true bmpint sth-4)
             (integer->octets sth-4 (arithmetic-shift mpint -32))]
            [(>= sth-1 offset)
             (bytes-set! bmpint sth-1 (bitwise-and mpint #xFF))
             (integer->octets sth-1 (arithmetic-shift mpint -8))]))
    
    bmpint))

(define network-bytes->integer : (->* (Bytes) (Natural Natural) Integer)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (assert (if (<= end0 start) (bytes-length bmpint) end0) index?))

    (let octets->integer ([idx : Index (assert start index?)]
                          [x : Integer (if (>= (bytes-ref bmpint start) #b10000000) -1 0)])
      (define idx+8 : Nonnegative-Fixnum (+ idx 8))
      (define idx+4 : Nonnegative-Fixnum (+ idx 4))
      (define idx+1 : Nonnegative-Fixnum (+ idx 1))
      
      (cond [(<= idx+8 end) (octets->integer idx+8 (bitwise-ior (arithmetic-shift x 64) (integer-bytes->integer bmpint #false #true idx idx+8)))]
            [(<= idx+4 end) (octets->integer idx+4 (bitwise-ior (arithmetic-shift x 32) (integer-bytes->integer bmpint #false #true idx idx+4)))]
            [(<= idx+1 end) (octets->integer idx+1 (bitwise-ior (arithmetic-shift x 8) (bytes-ref bmpint idx)))]
            [else x]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bits-bytes-length : (-> Natural Index)
  (lambda [bits]
    (assert (quotient (+ bits 7) 8) index?)))

(define integer-bytes-length : (-> Integer Index)
  (lambda [mpint]
    (bits-bytes-length (+ (integer-length mpint) 1 #|for sign bit|#))))
