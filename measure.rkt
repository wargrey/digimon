#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Length-Unit (U 'px 'cm 'mm 'Q 'in 'pc 'pt 'apc 'pls))
(define-type Angle-Unit (U 'deg 'rad 'grad 'turn))

(struct ~% ([datum : Real]) #:transparent)
(struct ~L ([datum : Real] [unit : Length-Unit]) #:transparent)

(define-type ~Real (U Real ~%))
(define-type ~Length (U Real ~L))
(define-type ~Length+% (U Real ~% ~L))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~px : (case-> [~L -> Nonnegative-Flonum]
                      [Real Length-Unit -> Nonnegative-Flonum]
                      [~Real Length-Unit Nonnegative-Flonum -> Nonnegative-Flonum])
  (case-lambda
    [(self) (~px (~L-datum self) (~L-unit self))]
    [(len unit)
     (define px (~dimension len))
     
     (case/eq unit
       [(px)       px]
       [(cm)       (* (/ 96.0 2.54) px)]
       [(mm)       (* (/ 96.0 25.4) px)]           #;1cm/10
       [(in)       (* 96.0 px)]
       [(pt)       (* (/ 96.0 72.0) px)]           #;1in/72
       [(pc)       (* 16.0 px)]                    #;1in/6
       [(Q)        (* (/ 96.0 101.6) px)]          #;1cm/40
       [(apc)      (* (* (/ 96.0 2.54) 3.086) px)] #;3.086cm
       [(pls)      (* 1.133 px)])]
    [(len unit 100%) (~px (~dimension len 100%) unit)]))

(define ~rad : (case-> [Real Angle-Unit -> Flonum]
                       [~Real Angle-Unit Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(ang unit)
     (cond [(eq? unit 'deg) (* (real->double-flonum ang) (/ pi 180.0))]
           [(eq? unit 'rad) (real->double-flonum ang)]
           [(eq? unit 'grad) (* (real->double-flonum ang) (/ pi 200.0))]
           [else 'turn (* (real->double-flonum ang) 2.0 pi)])]
    [(ang unit 100%) (~rad (~distance ang 100%) unit)]))

(define ~deg : (case-> [Real Angle-Unit -> Flonum]
                       [~Real Angle-Unit Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(ang unit)
     (cond [(eq? unit 'deg) (real->double-flonum ang)]
           [(eq? unit 'rad) (* (real->double-flonum ang) (/ 180.0 pi))]
           [(eq? unit 'grad) (* (real->double-flonum ang) 0.9)]
           [(eq? unit 'turn) (* (real->double-flonum ang) 360.0)])]
    [(ang unit 100%) (~deg (~distance ang 100%) unit)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~distance : (case-> [Real -> Flonum]
                            [~Real Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(fl) (real->double-flonum fl)]
    [(fl% 100%)
     (cond [(~%? fl%)
            (let ([v (real->double-flonum (~%-datum fl%))])
              (if (rational? v) (* v 0.01 100%) 100%))]
           [(rational? fl%) (real->double-flonum fl%)]
           [else #| nan or invalid value |# 100%])]))

(define ~dimension : (case-> [~Length -> Nonnegative-Flonum]
                             [~Length+% Nonnegative-Flonum -> Nonnegative-Flonum]
                             [~Length+% Nonnegative-Flonum (-> Nonnegative-Flonum) -> Nonnegative-Flonum])
  (case-lambda
    [(fl)
     (cond [(~L? fl) (~dimension (~px (~L-datum fl) (~L-unit fl)))]
           [(and (> fl 0.0) (< fl +inf.0)) (real->double-flonum fl)]
           [else 0.0])]
    [(fl 100%)
     (cond [(~L? fl) (~dimension (~px (~L-datum fl) (~L-unit fl)) 100%)]
           [(~%? fl)
            (let ([ratio (* (real->double-flonum (~%-datum fl)) 0.01)])
              (if (and (>= ratio 0.0) (< ratio +inf.0))
                  (* ratio 100%)
                  100%))]
           [(and (>= fl 0.0) (< fl +inf.0))
            (real->double-flonum fl)]
           [else #| nan or invalid value |# 100%])]
    [(fl 100% fallback)
     (cond [(~L? fl) (~dimension (~px (~L-datum fl) (~L-unit fl)) 100% fallback)]
           [(~%? fl)
            (let ([ratio (* (real->double-flonum (~%-datum fl)) 0.01)])
              (if (and (> ratio 0.0) (< ratio +inf.0))
                  (* ratio 100%)
                  (fallback)))]
           [(and (>= fl 0.0) (< fl +inf.0))
            (real->double-flonum fl)]
           [else #| nan or invalid value |# (fallback)])]))

(define ~extent : (case-> [~Length -> Nonnegative-Flonum]
                          [~Length (Option ~Length+%) -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(w) (~dimension w)]
    [(w h) (let ([width (~dimension w)])
             (cond [(not h) (values width width)]
                   [else (values width (~dimension h width))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~clamp : (case-> [Real Nonnegative-Real Nonnegative-Real -> Nonnegative-Flonum]
                         [Real Real Real -> Flonum]
                         [Nonnegative-Real Nonnegative-Real -> Nonnegative-Flonum]
                         [Real Nonnegative-Real -> Flonum])
  (case-lambda
    [(x range) (real->double-flonum (if (>= x range) range (max x (- range))))]
    [(x m M) (real->double-flonum (if (>= x M) M (max x m)))]))

(define ~lerp : (case-> [Nonnegative-Real Nonnegative-Real Nonnegative-Real -> Nonnegative-Flonum]
                        [Real Real Nonnegative-Real -> Flonum])
  (lambda [m M t]
    (define-values (flmin flmax) (values (real->double-flonum m) (real->double-flonum M)))
    (define range (- flmax flmin))

    (if (>= range 0.0)
        (+ (* range (real->double-flonum t)) flmin)
        (~lerp M m t))))

(define ~wrap : (->* (Real Real) (Real) Flonum)
  (lambda [datum range [start 0.0]]
    (define flrange (real->double-flonum range))
    (define flstart (real->double-flonum start))
    (define flend (+ flstart flrange))
    (define fldatum (real->double-flonum datum))
    
    (cond [(and (<= flstart fldatum) (< fldatum flend)) fldatum]
          [(< fldatum flstart) (let transform ([v (+ fldatum flrange)]) (if (>= v flstart) v (transform (+ v flrange))))]
          [else (let transform ([v (- fldatum flrange)]) (if (< v flend) v (transform (- v flrange))))])))
