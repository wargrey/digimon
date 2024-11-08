#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~radian : (case-> [Real -> Flonum]
                          [Real Boolean -> Flonum])
  (case-lambda
    [(degree) (* (real->double-flonum degree) (/ pi 180.0))]
    [(angle radian?) (if (and radian?) (real->double-flonum angle) (~radian angle))]))

(define ~length : (case-> [Real -> Nonnegative-Flonum]
                          [Real Nonnegative-Flonum -> Nonnegative-Flonum])
  (case-lambda
    [(fl) (if (> fl 0.0) (real->double-flonum fl) 0.0)]
    [(fl% 100%)
     (cond [(< fl% 0.0) (* (abs (real->double-flonum fl%)) 100%)]
           [(>= fl% 0.0) (real->double-flonum fl%)]
           [else #| nan |# 0.0])]))

(define ~size : (case-> [Real -> Nonnegative-Flonum]
                        [Real Real -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(w) (~length w)]
    [(w h) (let ([width (~length w)]) (values width (~length h width)))]))

(define ~clamp : (case-> [Nonnegative-Real Nonnegative-Real -> Nonnegative-Flonum]
                         [Real Nonnegative-Real -> Flonum])
  (lambda [x range]
    (real->double-flonum
     (if (>= x range) (min x range) (max x (- range))))))

(define ~wrap : (->* (Real Real) (Real) Flonum)
  (lambda [datum range [start 0.0]]
    (define flrange (real->double-flonum range))
    (define flstart (real->double-flonum start))
    (define flend (+ flstart flrange))
    (define fldatum (real->double-flonum datum))
    
    (cond [(and (<= flstart fldatum) (< fldatum flend)) fldatum]
          [(< fldatum flstart) (let transform ([v (+ fldatum flrange)]) (if (>= v flstart) v (transform (+ v flrange))))]
          [else (let transform ([v (- fldatum flrange)]) (if (< v flend) v (transform (- v flrange))))])))
