#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flc-ri : (-> Flonum Float-Complex)
  (lambda [r]
    (make-rectangular 0.0 r)))

(define flc-dot* : (-> Float-Complex Float-Complex Flonum)
  (lambda [z1 z2]
    (real-part (* z1 (conjugate z2)))))

(define flc-cross* : (-> Float-Complex Float-Complex Flonum)
  (lambda [z1 z2]
    (imag-part (* z1 (conjugate z2)))))

(define flc-rotate : (case-> [Float-Complex Flonum -> Float-Complex]
                             [Float-Complex Flonum Float-Complex -> Float-Complex])
  (case-lambda
    [(pt rad) (* pt (make-polar 1.0 rad))]
    [(pt rad origin) (+ origin (* (- pt origin) (make-polar 1.0 rad)))]))

(define flcs-rotate : (case-> [(Listof Float-Complex) Flonum -> (Listof Float-Complex)]
                              [(Listof Float-Complex) Flonum Float-Complex -> (Listof Float-Complex)])
  (case-lambda
    [(vertices rad)
     (let ([R (make-polar 1.0 rad)])
       (for/list : (Listof Float-Complex) ([v (in-list vertices)])
         (* v R)))]
    [(vertices rad origin)
     (let ([R (make-polar 1.0 rad)])
       (for/list : (Listof Float-Complex) ([v (in-list vertices)])
         (+ origin (* (- v origin) R))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flc-round : (-> Float-Complex Float-Complex)
  (lambda [pt]
    (make-rectangular (round (real-part pt))
                      (round (imag-part pt)))))

(define flc-floor : (-> Float-Complex Float-Complex)
  (lambda [pt]
    (make-rectangular (floor (real-part pt))
                      (floor (imag-part pt)))))

(define flc-ceiling : (-> Float-Complex Float-Complex)
  (lambda [pt]
    (make-rectangular (ceiling (real-part pt))
                      (ceiling (imag-part pt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flc-interval : (->* ((Listof Float-Complex))
                            (Flonum Flonum Flonum Flonum)
                            (Values Flonum Flonum Flonum Flonum))
  (lambda [pts [rmin0 +inf.0] [imin0 +inf.0] [rmax0 -inf.0] [imax0 -inf.0]]
    (let interval ([rmin : Flonum rmin0]
                   [rmax : Flonum rmax0]
                   [imin : Flonum imin0]
                   [imax : Flonum imax0]
                   [pts : (Listof Float-Complex) pts])
      (if (pair? pts)
          (let ([r (real-part (car pts))]
                [i (imag-part (car pts))])
            (interval (min rmin r) (max rmax r)
                      (min imin i) (max imax i)
                      (cdr pts)))
          (values rmin imin rmax imax)))))

(define flc-real-interval : (->* ((Listof Float-Complex)) (Flonum Flonum) (Values Flonum Flonum))
  (lambda [pts [min0 +inf.0] [max0 -inf.0]]
    (let interval ([rmin : Flonum min0]
                   [rmax : Flonum max0]
                   [pts : (Listof Float-Complex) pts])
      (if (pair? pts)
          (let ([r (real-part (car pts))])
            (interval (min rmin r) (max rmax r) (cdr pts)))
          (values rmin rmax)))))

(define flc-imag-interval : (->* ((Listof Float-Complex)) (Flonum Flonum) (Values Flonum Flonum))
  (lambda [pts [min0 +inf.0] [max0 -inf.0]]
    (let interval ([imin : Flonum min0]
                   [imax : Flonum max0]
                   [pts : (Listof Float-Complex) pts])
      (if (pair? pts)
          (let ([i (imag-part (car pts))])
            (interval (min imin i) (max imax i) (cdr pts)))
          (values imin imax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flc-rational? : (-> Complex Boolean)
  (lambda [c]
    (and (rational? (real-part c))
         (rational? (imag-part c)))))
