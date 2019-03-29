#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/unsafe/ops)

(define padding : Positive-Byte #;#\= #x3D)
(define 62nd : Positive-Byte #;#\+ #x2B)
(define 63rd : Positive-Byte #;#\/ #x2F)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define encode : (-> Bytes Positive-Byte Positive-Byte Bytes)
  (lambda [src 62nd 63rd]
    (define ssize : Index (bytes-length src))
    (define-values (q r) (quotient/remainder ssize 3))
    (define dest : Bytes (make-bytes (unsafe-fx* 4 (+ q (min r 1))) padding))
    (define dsize : Index (bytes-length dest))

    (unsafe-vector*-set! b2t-table 62 62nd)
    (unsafe-vector*-set! b2t-table 63 63rd)
    
    (let translate ([sidx : Nonnegative-Fixnum 0]
                    [didx : Nonnegative-Fixnum 0])
      (when (and (< sidx ssize) (< didx dsize))
        (binary-to-text src sidx (- ssize sidx) dest didx)
        (translate (+ sidx 3) (+ didx 4))))
    
    dest))

(define decode : (-> Bytes Positive-Byte Positive-Byte Bytes)
  (lambda [src 62nd 63rd]
    (define spsize : Index (bytes-length src))
    (define ssize : Index
      (cond [(< spsize 3) spsize]
            [(= (unsafe-bytes-ref src (- spsize 2)) padding) (assert (- spsize 2) index?)]
            [(= (unsafe-bytes-ref src (- spsize 1)) padding) (assert (- spsize 1) index?)]
            [else spsize]))

    (define-values (q r) (quotient/remainder ssize 4))
    (define dest : Bytes (make-bytes (unsafe-fx+ (unsafe-fx* 3 q) (case r [(3) 2] [(2) 1] [else 0]))))
    (define dsize : Index (bytes-length dest))

    (unsafe-vector*-set! t2b-table 62nd 62)
    (unsafe-vector*-set! t2b-table 63rd 63)
    
    (let translate ([sidx : Nonnegative-Fixnum 0]
                    [didx : Nonnegative-Fixnum 0])
      (when (and (< sidx ssize) (< didx dsize))
        (text-to-binary src sidx (- ssize sidx) dest didx)
        (translate (+ sidx 4) (+ didx 3))))
    
    dest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (b2t-table t2b-table)
  (let ([ch64s (append (range #x41 #x5B) #;A-Z (range #x61 #x7B) #;a-z (range #x30 #x3A) #;0-9 (list 62nd 63rd))]
        [b2t-table (make-vector 64 0)] [t2b-table (make-vector #xFF 0)])
    (for ([ch (in-list ch64s)]
          [idx (in-naturals)])
      (vector-set! b2t-table idx ch)
      (vector-set! t2b-table ch idx))
    (values b2t-table t2b-table)))

(define binary-to-text : (-> Bytes Index Fixnum Bytes Index Void)
  (lambda [src sidx rest dest didx]
    (case rest
      [(1)
       (let ([bits (unsafe-fxlshift (unsafe-bytes-ref src sidx) 4)])
         (unsafe-bytes-set! dest (+ didx 1) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 0 6)))
         (unsafe-bytes-set! dest (+ didx 0) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 6 12))))]
      [(2)
       (let ([bits (bitwise-ior (unsafe-fxlshift (unsafe-bytes-ref src sidx) 10) (unsafe-fxlshift (unsafe-bytes-ref src (+ sidx 1)) 2))])
         (unsafe-bytes-set! dest (+ didx 2) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 0 6)))
         (unsafe-bytes-set! dest (+ didx 1) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 6 12)))
         (unsafe-bytes-set! dest (+ didx 0) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 12 18))))]
      [else
       (let ([bits (bitwise-ior (unsafe-fxlshift (unsafe-bytes-ref src (+ sidx 0)) 16)
                                (unsafe-fxlshift (unsafe-bytes-ref src (+ sidx 1)) 8)
                                (unsafe-bytes-ref src (+ sidx 2)))])
         (unsafe-bytes-set! dest (+ didx 3) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits  0  6)))
         (unsafe-bytes-set! dest (+ didx 2) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits  6 12)))
         (unsafe-bytes-set! dest (+ didx 1) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 12 18)))
         (unsafe-bytes-set! dest (+ didx 0) (unsafe-vector*-ref b2t-table (bitwise-bit-field bits 18 24))))])))

(define text-to-binary : (-> Bytes Index Integer Bytes Index Void)
  (lambda [src sidx rest dest didx]
    (case rest
      [(3)
       (let ([bits (bitwise-ior (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 0))) 10)
                                (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 1)))  4)
                                (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 2))) -2))])
         (unsafe-bytes-set! dest (+ didx 1) (bitwise-bit-field bits  0  8))
         (unsafe-bytes-set! dest (+ didx 0) (bitwise-bit-field bits  8 16)))]
      [(2)
       (let ([bits (bitwise-ior (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 0)))  2)
                                (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 1))) -4))])
         (unsafe-bytes-set! dest (+ didx 0) (bitwise-bit-field bits  0  8)))]
      [else
       (let ([bits (bitwise-ior (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 0))) 18)
                                (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 1))) 12)
                                (unsafe-fxlshift (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 2)))  6)
                                (unsafe-vector*-ref t2b-table (unsafe-bytes-ref src (+ sidx 3))))])
         (unsafe-bytes-set! dest (+ didx 2) (bitwise-bit-field bits  0  8))
         (unsafe-bytes-set! dest (+ didx 1) (bitwise-bit-field bits  8 16))
         (unsafe-bytes-set! dest (+ didx 0) (bitwise-bit-field bits 16 24)))])))
