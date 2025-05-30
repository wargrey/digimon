#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))
(define nonnegative-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [f] (and (flonum? f) (>= f 0.0)))) ; unable to deal with +nan.0

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [b] (and (byte? b) (> b 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [i] (and (index? i) (> i 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))
(define positive-flonum? : (-> Any Boolean : #:+ Positive-Flonum) (λ [f] (and (flonum? f) (> f 0.0)))) ; unable to deal with +nan.0

(define fixed-percentage? : (-> Any Boolean : #:+ Flonum) (λ [%] (and (flonum? %) (<= -100.0 % 100.0))))
(define nonnegative-fixed-percentage? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [%] (and (flonum? %) (>= % 0.0) (<= % 100.0))))

(define normalized-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [f] (and (flonum? f) (>= f 0.0) (<= f 1.0))))

(define exact-rational? : (-> Any Boolean : Exact-Rational) (λ [f] (and (rational? f) (exact? f))))
(define exact-fraction? : (-> Any Boolean : #:+ Exact-Rational) (λ [f] (and (rational? f) (exact? f) (not (exact-integer? f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define real-zero? : (-> Any Boolean)
  (lambda [v]
    (and (real? v)
         (zero? v))))
