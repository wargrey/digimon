#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define λfalse : (-> Any * False)
  (lambda who-cares
    #false))

(define λnan : (-> Any * Nonnegative-Flonum)
  (lambda who-cares
    +nan.0))
