#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define use-bytes+offset : (->* ((Option Bytes) Natural Natural) (Byte Boolean) (values Bytes Index))
  (lambda [pool0 size offset0 [b #x00] [fill? #false]]
    (cond [(not pool0) (values (make-bytes size b) 0)]
          [else (let* ([end-idx (bytes-length pool0)]
                       [start (if (< offset0 end-idx) offset0 end-idx)])
                  (unless(not fill?)
                    (for ([idx (in-range start (+ start size))])
                      (bytes-set! pool0 idx b)))
                  (values pool0 start))])))

(define bytes-range-end : (-> Bytes Natural Natural Index)
  (lambda [bs start end-hint]
    ; NOTE: this implementation may conceal bugs when decoding integers following mismatched length tag
    (define end-max : Index (bytes-length bs))
    (cond [(<= end-hint start) end-max]
          [(<= end-hint end-max) end-hint]
          [else end-max])))

(define unsafe-bytes-range-end : (-> Bytes Natural Natural Index)
  (lambda [bs start end-hint]
    (define end-max : Index (bytes-length bs))
    (cond [(<= end-hint start) end-max]
          [else (assert end-hint index?)])))

(define network-natural-bytes++ : (->* (Bytes) (Natural Natural) Void)
  (lambda [mpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end mpint start end0))

    (let i++ ([idx : Fixnum (- end 1)])
      (when (>= idx start)
        (let ([v (bytes-ref mpint idx)])       
          (cond [(< v #xFF) (bytes-set! mpint idx (+ v 1))]
                [else (bytes-set! mpint idx 0)
                      (i++ (- idx 1))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define integer->network-bytes : (->* (Integer) (Index Bytes Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define isize : Index (integer-bytes-length mpint))
    (define bsize : Index (if (<= isize bsize0) bsize0 isize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (let integer->octets ([sth : Nonnegative-Fixnum (+ bsize offset)]
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
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (let octets->integer ([idx : Index (assert start index?)]
                          [x : Integer (if (>= (bytes-ref bmpint start) #b10000000) -1 0)])
      (define idx+8 : Nonnegative-Fixnum (+ idx 8))
      (define idx+4 : Nonnegative-Fixnum (+ idx 4))
      (define idx+1 : Nonnegative-Fixnum (+ idx 1))
      
      (cond [(<= idx+8 end) (octets->integer idx+8 (bitwise-ior (arithmetic-shift x 64) (integer-bytes->integer bmpint #false #true idx idx+8)))]
            [(<= idx+4 end) (octets->integer idx+4 (bitwise-ior (arithmetic-shift x 32) (integer-bytes->integer bmpint #false #true idx idx+4)))]
            [(<= idx+1 end) (octets->integer idx+1 (bitwise-ior (arithmetic-shift x 8) (bytes-ref bmpint idx)))]
            [else x]))))

(define natural->network-bytes : (->* (Natural) (Index Bytes Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define nsize : Index (natural-bytes-length mpint))
    (define bsize : Index (if (<= nsize bsize0) bsize0 nsize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (let integer->octets ([sth : Nonnegative-Fixnum (+ bsize offset)]
                          [mpint : Natural mpint])
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

(define network-bytes->natural : (->* (Bytes) (Natural Natural) Natural)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (let octets->integer ([idx : Index (assert start index?)]
                          [x : Natural 0])
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

(define natural-bytes-length : (-> Natural Index)
  (lambda [mpint]
    (bits-bytes-length (integer-length mpint))))