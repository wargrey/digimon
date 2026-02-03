#lang typed/racket/base

(provide (all-defined-out) pi)
(provide (rename-out [pi/2 +pi/2] [3pi/2 +3pi/2] [pi/4 +pi/4] [3pi/4 +3pi/4]
                     [pi/5 +pi/5] [2pi/5 +2pi/5] [3pi/5 +3pi/5] [4pi/5 +4pi/5] [pi +pi]
                     [phi +phi] [1/phi +1/phi]))
(provide (rename-out [pi/3 +pi/3] [2pi/3 +2pi/3] [pi/6 +pi/6] [5pi/6 +5pi/6]
                     [pi/8 +pi/8] [3pi/8 +3pi/8] [5pi/8 +5pi/8] [7pi/8 +7pi/8]
                     [pi/12 +pi/12] [5pi/12 +5pi/12] [7pi/12 +7pi/12] [11pi/12 +11pi/12]))

(require racket/math)
(require racket/case)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (-pi/2 pi/2 -3pi/2 3pi/2 -2pi 2pi -pi/4 pi/4 -3pi/4 3pi/4 -2pi/5 pi/5 -pi/5 2pi/5 -3pi/5 3pi/5 -4pi/5 4pi/5 -pi)
  (values (* pi -0.50) (* pi +0.50) (* pi -1.50) (* pi +1.50) (* pi -2.0) (* pi +2.0)
          (* pi -0.25) (* pi +0.25) (* pi -0.75) (* pi +0.75)
          (* pi -0.20) (* pi +0.20) (* pi -0.40) (* pi +0.40) (* pi -0.60) (* pi +0.60) (* pi -0.80) (* pi +0.80)
          (- pi)))

(define-values (-pi/3 pi/3 -2pi/3 2pi/3 -pi/6 pi/6 -5pi/6 5pi/6 -pi/8 pi/8 -3pi/8 3pi/8 -5pi/8 5pi/8 -7pi/8 7pi/8)
  (values (* pi -1/3) (* pi 1/3) (* pi -2/3) (* pi +2/3)
          (* pi -1/6) (* pi 1/6) (* pi -5/6) (* pi +5/6)
          (* pi -1/8) (* pi 1/8) (* pi -3/8) (* pi +3/8) (* pi -5/8) (* pi 5/8) (* pi -7/8) (* pi +7/8)))

(define-values (-pi/12 pi/12 -5pi/12 5pi/12 -7pi/12 7pi/12 -11pi/12 11pi/12)
  (values (* pi -1/12) (* pi +1/12) (* pi -5/12) (* pi +5/12)
          (* pi -7/12) (* pi +7/12) (* pi -11/12) (* pi +11/12)))

(define phi : Nonnegative-Flonum (* (+ 1.0 (sqrt 5.0)) 0.5))
(define 1/phi : Nonnegative-Flonum (/ 2.0 (+ 1.0 (sqrt 5.0))))
(define -phi : Flonum (- phi))
(define -1/phi : Flonum (- 1/phi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Length-Unit (U 'px 'cm 'mm 'Q 'in 'pc 'pt 'apc 'pls))
(define-type Font-Relative-Length-Unit (U 'em 'ex 'cap 'ch 'ic))
(define-type Angle-Unit (U 'deg 'rad 'grad 'turn))

(define-type Real+% (U Real &:))
(define-type Length+% (U Real &L &:))
(define-type Real-Length (U Real &L))

(struct &L ([datum : Real] [unit : (U Length-Unit Font-Relative-Length-Unit)]) #:transparent)
(struct &: ([datum : Real]) #:transparent)
(define (&% [datum : Real]) (&: (* datum 1/100)))

(define real-length? : (-> Any Boolean : Real-Length) (λ [v] (or (real? v) (&L? v))))
(define real+%? : (-> Any Boolean : Real+%) (λ [v] (or (real? v) (&:? v))))
(define length+%? : (-> Any Boolean : Length+%) (λ [v] (or (real? v) (&L? v) (&:? v))))

(define &rational? : (-> Any Boolean)
  (lambda [v]
    (cond [(real? v) (rational? v)]
          [(&:? v) (rational? (&:-datum v))]
          [(&L? v) (rational? (&L-datum v))]
          [else #false])))

(define &zero? : (-> Any Boolean)
  (lambda [v]
    (cond [(real? v) (zero? v)]
          [(&:? v) (zero? (&:-datum v))]
          [(&L? v) (zero? (&L-datum v))]
          [else #false])))

(define range% : (case-> [Real -> (Listof &:)]
                         [Real Real -> (Listof &:)]
                         [Real Real Real -> (Listof &:)])
  (case-lambda
    [(end)
     (for/list ([p (in-range 0 end 1)]) (&% p))]
    [(start end)
     (for/list ([p (in-range start end 1)]) (&% p))]
    [(start end step)
     (for/list ([p (in-range start end step)]) (&% p))]))

(define default-font-metrics : (Parameterof (U (Listof (Pairof Symbol Nonnegative-Flonum))
                                               (-> Font-Relative-Length-Unit Nonnegative-Flonum)))
  (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~px : (case-> [&L -> Flonum]
                      [Real (U Length-Unit Font-Relative-Length-Unit) -> Flonum]
                      [Real+% (U Length-Unit Font-Relative-Length-Unit) Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(dim) (~px (&L-datum dim) (&L-unit dim))]
    [(len unit)
     (define px (~distance len))
     
     (case/eq unit
       [(px)       px]
       [(cm)       (* (/ 96.0 2.54) px)]
       [(mm)       (* (/ 96.0 25.4) px)]           #;1cm/10
       [(in)       (* 96.0 px)]
       [(pt)       (* (/ 96.0 72.0) px)]           #;1in/72
       [(pc)       (* 16.0 px)]                    #;1in/6
       [(Q)        (* (/ 96.0 101.6) px)]          #;1cm/40
       [(apc)      (* (* (/ 96.0 2.54) 3.086) px)] #;3.086cm
       [(pls)      (* 1.133 px)]
       [else (let ([frl (default-font-metrics)])
               (if (list? frl)
                   (let ([val (assq unit frl)])
                     (if (not val)
                         (raise (make-exn:fail:unsupported
                                 (format "Failed to resolve the font relative length: ~a" unit)
                                 (current-continuation-marks)))
                         (* px (cdr val))))
                   (case/eq unit
                     [(em)       (* px (frl 'em))]
                     [(ex)       (* px (frl 'ex))]
                     [(ch)       (* px (frl 'ch))]
                     [(ic)       (* px (frl 'ic))]
                     [(cap)      (* px (frl 'cap))])))])]
    [(len unit ratio-to) (~px (~distance len ratio-to) unit)]))

(define ~rad : (case-> [Real Angle-Unit -> Flonum]
                       [Real+% Angle-Unit Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(ang unit)
     (case/eq unit
       [(rad) (real->double-flonum ang)]
       [(deg) (* (real->double-flonum ang) (/ pi 180.0))]
       [(grad) (* (real->double-flonum ang) (/ pi 200.0))]
       [(turn) (* (real->double-flonum ang) 2.0 pi)])]
    [(ang unit ratio-to) (~rad (~distance ang ratio-to) unit)]))

(define ~deg : (case-> [Real Angle-Unit -> Flonum]
                       [(U Real &:) Angle-Unit Nonnegative-Flonum -> Flonum])
  (case-lambda
    [(ang unit)
     (case/eq unit
       [(deg) (real->double-flonum ang)]
       [(rad) (* (real->double-flonum ang) (/ 180.0 pi))]
       [(grad) (* (real->double-flonum ang) 0.9)]
       [(turn) (* (real->double-flonum ang) 360.0)])]
    [(ang unit ratio-to) (~deg (~distance ang ratio-to) unit)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~distance : (case-> [Real-Length -> Flonum]
                            [Length+% Flonum -> Flonum])
  (case-lambda
    [(fl) (if (&L? fl) (~distance (~px fl)) (real->double-flonum fl))]
    [(fl ratio-to)
     (cond [(rational? fl) (real->double-flonum fl)]
           [(&L? fl) (~distance (~px (&L-datum fl) (&L-unit fl)) ratio-to)]
           [(&:? fl)
            (let ([v (real->double-flonum (&:-datum fl))])
              (if (rational? v) (* v ratio-to) ratio-to))]
           [else #| nan or invalid value |# ratio-to])]))

(define ~placement : (-> Length+% Nonnegative-Flonum Nonnegative-Flonum)
  (lambda [fl span]
    (cond [(real? fl)
           (cond [(> fl span) (~placement (- fl span) span)]
                 [(>= fl 0.0) (real->double-flonum fl)]
                 [else (~placement (+ span fl) span)])]
          [(&L? fl) (~placement (~px (&L-datum fl) (&L-unit fl)) span)]
          [(&:? fl) (~placement (* (&:-datum fl) span) span)])))

(define ~dimension : (case-> [Real-Length -> Nonnegative-Flonum]
                             [Length+% Nonnegative-Flonum -> Nonnegative-Flonum]
                             [Length+% Nonnegative-Flonum (-> Nonnegative-Flonum) -> Nonnegative-Flonum])
  (case-lambda
    [(fl)
     (cond [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)))]
           [(and (> fl 0.0) (< fl +inf.0)) (real->double-flonum fl)]
           [else 0.0])]
    [(fl ratio-to)
     (cond [(real? fl) (if (and (>= fl 0.0) (< fl +inf.0)) (real->double-flonum fl) ratio-to)]
           [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)) ratio-to)]
           [(&:? fl)
            (let ([ratio (real->double-flonum (&:-datum fl))])
              (if (and (>= ratio 0.0) (< ratio +inf.0))
                  (* ratio ratio-to)
                  ratio-to))])]
    [(fl ratio-to fallback)
     (cond [(real? fl) (if (and (>= fl 0.0) (< fl +inf.0)) (real->double-flonum fl) (fallback))]
           [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)) ratio-to fallback)]
           [(&:? fl)
            (let ([ratio (real->double-flonum (&:-datum fl))])
              (if (and (>= ratio 0.0) (< ratio +inf.0))
                  (* ratio ratio-to)
                  (fallback)))])]))

(define ~dimension/inf : (case-> [Real-Length -> Nonnegative-Flonum]
                                 [Length+% Nonnegative-Flonum -> Nonnegative-Flonum]
                                 [Length+% Nonnegative-Flonum (-> Nonnegative-Flonum) -> Nonnegative-Flonum])
  (case-lambda
    [(fl)
     (cond [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)))]
           [(and (>= fl 0.0) (<= fl +inf.0)) (real->double-flonum fl)]
           [else 0.0])]
    [(fl ratio-to)
     (cond [(real? fl) (if (and (>= fl 0.0) (<= fl +inf.0)) (real->double-flonum fl) ratio-to)]
           [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)) ratio-to)]
           [(&:? fl)
            (let ([ratio (real->double-flonum (&:-datum fl))])
              (if (and (>= ratio 0.0) (<= ratio +inf.0))
                  (* ratio ratio-to)
                  ratio-to))])]
    [(fl ratio-to fallback)
     (cond [(real? fl) (if (and (>= fl 0.0) (<= fl +inf.0)) (real->double-flonum fl) (fallback))]
           [(&L? fl) (~dimension (~px (&L-datum fl) (&L-unit fl)) ratio-to fallback)]
           [(&:? fl)
            (let ([ratio (real->double-flonum (&:-datum fl))])
              (if (and (>= ratio 0.0) (<= ratio +inf.0))
                  (* ratio ratio-to)
                  (fallback)))])]))

(define ~extent : (case-> [Real-Length -> Nonnegative-Flonum]
                          [Real-Length (Option Length+%) -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
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
