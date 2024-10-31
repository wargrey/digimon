#lang typed/racket/base

(provide (all-defined-out) pi)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (-pi/2 pi/2 3pi/2 2pi pi/4 3pi/4 2pi/5 4pi/5)
  (values (* pi -0.5) (* pi +0.5)
          (* pi +1.5) (* pi +2.0)
          (* pi 0.25) (* pi 0.75)
          (* pi 0.40) (* pi 0.80)))

(define phi : Nonnegative-Flonum (* (+ 1.0 (sqrt 5.0)) 0.5))
(define 1/phi : Nonnegative-Flonum (/ 2.0 (+ 1.0 (sqrt 5.0))))
(define -phi : Flonum (- phi))
(define -1/phi : Flonum (- 1/phi))
