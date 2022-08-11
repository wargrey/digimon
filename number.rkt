#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [string+>integer string->natural]))
(provide msb-bytes->float lsb-bytes->float
         msb-bytes->double lsb-bytes->double)

(require "digitama/unsafe/number.rkt")
(require "digitama/number.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))
(define nonnegative-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [f] (and (flonum? f) (>= f 0.0)))) ; unable to deal with +nan.0

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [b] (and (byte? b) (> b 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [i] (and (index? i) (> i 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))
(define positive-flonum? : (-> Any Boolean : #:+ Positive-Flonum) (λ [f] (and (flonum? f) (> f 0.0)))) ; unable to deal with +nan.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string->integer : (-> String (Option Integer))
  (lambda [str.n]
    (define n (string->number str.n))

    (and (exact-integer? n) n)))

(define string+>integer : (-> String (Option Natural))
  (lambda [str.n]
    (define n (string->number str.n))

    (and (exact-nonnegative-integer? n) n)))

(define string->real : (-> String Real)
  (lambda [str.n]
    (define n (string->number str.n))

    (cond [(real? n) n]
          [else +nan.0])))

(define string+>real : (-> String Nonnegative-Real)
  (lambda [str.n]
    (define n (string->number str.n))

    (cond [(and (real? n) (>= n 0)) n]
          [else +nan.0])))

(define string->flonum : (-> String Flonum)
  (lambda [str.n]
    (real->double-flonum (string->real str.n))))

(define string+>flonum : (-> String Nonnegative-Flonum)
  (lambda [str.n]
    (real->double-flonum (string+>real str.n))))

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
(define-bytes->integer*
  [msb-bytes->octet [msb-bytes->int8  #:-> Fixnum]  [msb-bytes->uint8  #:-> Byte]]
  [msb-bytes->short [msb-bytes->int16 #:-> Fixnum]  [msb-bytes->uint16 #:-> Index]]
  [msb-bytes->int   [msb-bytes->int32 #:-> Fixnum]  [msb-bytes->uint32 #:-> Index]]
  [msb-bytes->long  [msb-bytes->int64 #:-> Integer] [msb-bytes->uint64 #:-> Natural]])

(define-bytes->integer*
  [lsb-bytes->octet [lsb-bytes->int8  #:-> Fixnum]  [lsb-bytes->uint8  #:-> Byte]]
  [lsb-bytes->short [lsb-bytes->int16 #:-> Fixnum]  [lsb-bytes->uint16 #:-> Index]]
  [lsb-bytes->int   [lsb-bytes->int32 #:-> Fixnum]  [lsb-bytes->uint32 #:-> Index]]
  [lsb-bytes->long  [lsb-bytes->int64 #:-> Integer] [lsb-bytes->uint64 #:-> Natural]])

(define-bytes->integer msb-bytes->size msb-bytes->index [size : Integer] #:-> Index)
(define-bytes->integer lsb-bytes->size lsb-bytes->index [size : Integer] #:-> Index)

(define-integer->bytes* fixed-integer->bytes #true
  [1 [msb-int8->bytes]  [msb-uint8->bytes]]
  [2 [msb-int16->bytes] [msb-uint16->bytes]]
  [4 [msb-int32->bytes] [msb-uint32->bytes]]
  [8 [msb-int64->bytes] [msb-uint64->bytes]])

(define-integer->bytes* fixed-integer->bytes #false
  [1 [lsb-int8->bytes]  [lsb-uint8->bytes]]
  [2 [lsb-int16->bytes] [lsb-uint16->bytes]]
  [4 [lsb-int32->bytes] [lsb-uint32->bytes]]
  [8 [lsb-int64->bytes] [lsb-uint64->bytes]])

(define msb-size->bytes : (case-> [-> Integer Byte Bytes]
                                  [->* (Integer Byte Bytes) (Natural) Natural])
  (case-lambda
    [(n size) (fixed-integer->bytes n size #false #true)]
    [(n size bs) (fixed-integer->bytes n size #false #true bs 0)]
    [(n size bs offset) (fixed-integer->bytes n size #false #true bs offset)]))

(define lsb-size->bytes : (case-> [-> Integer Byte Bytes]
                                  [->* (Integer Byte Bytes) (Natural) Natural])
  (case-lambda
    [(n size) (fixed-integer->bytes n size #false #false)]
    [(n size bs) (fixed-integer->bytes n size #false #false bs 0)]
    [(n size bs offset) (fixed-integer->bytes n size #false #false bs offset)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define integer->network-bytes : (->* (Integer) (Index (Option Bytes) Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define isize : Index (integer-bytes-length mpint))
    (define bsize : Index (if (<= isize bsize0) bsize0 isize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (integer->msb-octets mpint #: (Integer bsize) #:-> bmpint #:at offset)))

(define network-bytes->integer : (->* (Bytes) (Natural Natural) Integer)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (msb-octets->integer bmpint #:from start #:to end #:-> Integer
                         #:with (if (>= (bytes-ref bmpint start) #b10000000) -1 0))))

(define natural->network-bytes : (->* (Natural) (Index (Option Bytes) Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define nsize : Index (natural-bytes-length mpint))
    (define bsize : Index (if (<= nsize bsize0) bsize0 nsize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (integer->msb-octets mpint #: (Natural bsize) #:-> bmpint #:at offset)))

(define network-bytes->natural : (->* (Bytes) (Natural Natural) Natural)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (msb-octets->integer bmpint #:from start #:to end #:-> Natural #:with 0)))

(define integer->memory-bytes : (->* (Integer) (Index (Option Bytes) Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define isize : Index (integer-bytes-length mpint))
    (define bsize : Index (if (<= isize bsize0) bsize0 isize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (integer->lsb-octets mpint #: (Integer bsize) #:-> bmpint #:at offset)))

(define memory-bytes->integer : (->* (Bytes) (Natural Natural) Integer)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (lsb-octets->integer bmpint #:from start #:to end #:-> Integer
                         #:with (let ([sign-idx (- end 1)])
                                  (if (and (>= sign-idx 0)
                                           (>= (bytes-ref bmpint sign-idx) #b10000000))
                                      -1 0)))))

(define natural->memory-bytes : (->* (Natural) (Index (Option Bytes) Natural) Bytes)
  (lambda [mpint [bsize0 0] [bmpint0 #false] [offset0 0]]
    (define nsize : Index (natural-bytes-length mpint))
    (define bsize : Index (if (<= nsize bsize0) bsize0 nsize))
    (define-values (bmpint offset) (use-bytes+offset bmpint0 bsize offset0))
    
    (integer->lsb-octets mpint #: (Natural bsize) #:-> bmpint #:at offset)))

(define memory-bytes->natural : (->* (Bytes) (Natural Natural) Natural)
  (lambda [bmpint [start 0] [end0 0]]
    (define end : Index (unsafe-bytes-range-end bmpint start end0))

    (lsb-octets->integer bmpint #:from start #:to end #:-> Natural #:with 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bits-bytes-length : (-> Natural Index)
  (lambda [bits]
    (arithmetic-shift (assert (+ bits 7) index?) -3)))

(define integer-bytes-length : (-> Integer Index)
  (lambda [mpint]
    (bits-bytes-length (+ (integer-length mpint) 1 #|for sign bit|#))))

(define natural-bytes-length : (-> Natural Index)
  (lambda [mpint]
    (bits-bytes-length (integer-length mpint))))
