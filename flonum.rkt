#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flround/scaled : (->* (Flonum) (Nonnegative-Flonum) Flonum)
  (lambda [fl [precision 10000.0]]
    (round (* fl precision))))

(define flnear? : (->* (Flonum Flonum) (Flonum) Boolean)
  (lambda [x c [tolerance 1e-12]]
    (<= (abs (- x c)) tolerance)))

(define flbetween-inclusive? : (->* (Flonum Flonum Flonum) (Flonum) Boolean)
  (lambda [x min max [tolerance 1e-12]]
    (or (<= min x max)
        (<= (- min tolerance min)
            x
            (+ max tolerance)))))

(define flbetween-exclusive? : (->* (Flonum Flonum Flonum) (Flonum) Boolean)
  (lambda [x min max [tolerance 1e-12]]
    (or (< min x max)
        (< (+ min tolerance)
           x
           (- max tolerance)))))
