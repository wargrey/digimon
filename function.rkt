#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define λfalse : (-> Any * False)
  (lambda who-cares
    #false))

(define λnull : (-> Any * Null)
  (lambda who-cares
    null))

(define λvoid : (-> Any * Void)
  (lambda who-cares
    (void)))

(define λnan : (-> Any * Nonnegative-Flonum)
  (lambda who-cares
    +nan.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T D) λoption : (-> (-> T D) D (-> (Option T) D))
  (lambda [->v supplement]
    (λ [[datum : (Option T)]] : D
      (if (not datum) supplement (->v datum)))))
