#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))
(define nonnegative-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [f] (and (flonum? f) (>= f 0.0)))) ; unable to deal with +nan.0

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [b] (and (byte? b) (> b 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [i] (and (index? i) (> i 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))
(define positive-flonum? : (-> Any Boolean : #:+ Positive-Flonum) (λ [f] (and (flonum? f) (> f 0.0)))) ; unable to deal with +nan.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define real-zero? : (-> Any Boolean)
  (lambda [v]
    (and (real? v)
         (zero? v))))
