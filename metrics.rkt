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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (N) list->4:values : (-> (Listof N) N (Values N N N N))
  (lambda [ls defval]
    (cond [(null? ls) (values defval defval defval defval)]
          [(null? (cdr ls)) (let ([top (car ls)]) (values top top top top))]
          [(null? (cddr ls)) (let ([top (car ls)] [right (cadr ls)]) (values top right top right))]
          [(null? (cdddr ls)) (let ([top (car ls)] [right (cadr ls)] [bottom (caddr ls)]) (values top right bottom right))]
          [else (values (car ls) (cadr ls) (caddr ls) (cadddr ls))])))
