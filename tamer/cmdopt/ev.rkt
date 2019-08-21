#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)
(require racket/string)

(require digimon/cmdopt)
(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string->probability : (-> String (Option Flonum))
  (lambda [str.px]
    (define px : (Option Number) (string->number str.px))

    (cond [(and (real? px) (>= px 0.0)) (real->double-flonum px)]
          [else (let ([n/d (string-split str.px "/")])
                  (and (= (length n/d) 2)
                       (let ([numerator (string->number (car n/d))]
                             [denominator (string->number (cadr n/d))])
                         (and (exact-nonnegative-integer? numerator)
                              (exact-nonnegative-integer? denominator)
                              (real->double-flonum (/ numerator denominator))))))])))

(define pmf-col : (-> String String (Pairof Flonum Flonum))
  (lambda [str.x str.px]
    (define x : (Option Number) (string->number str.x))
    (define p : (Option Flonum) (string->probability str.px))

    (cond [(and (real? x) p) (cons (real->double-flonum x) p)]
          [else (cmdopt-error 'ev "expected random variable, given (~a ~a)" str.x str.px)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option ev-flags #: EV-Flags
  #:usage-help "compute the E(X), E(X²), V(X) and σ"
  #:multi
  [[(#\p pmf) #:=> pmf-col x px #: (Pairof Flonum Flonum)
              "the table of pmf"]])

(define-values (options λargv) (parse-ev-flags))

(with-handlers ([exn:fail:user? (λ [[e : exn:fail:user]] (display-ev-flags #:user-error e #:exit 1))])
  (define pmf : (Listof (Pairof Flonum Flonum)) (ev-flags-pmf options))
  (when (null? pmf) (cmdopt-error 'EV "empty sample"))

  (define xs : (Listof Flonum) (map (inst car Flonum Flonum) pmf))
  (define ps : (Listof Flonum) (map (inst cdr Flonum Flonum) pmf))
  
  (define μ : Flonum  (foldl (λ [[x : Flonum] [p : Flonum] [Σ : Flonum]] (+ Σ (* x p))) 0.0 xs ps))
  (define Ex² : Flonum (foldl (λ [[x : Flonum] [p : Flonum] [Σ : Flonum]] (+ Σ (* x x p))) 0.0 xs ps))
  (define σ² : Flonum (foldl (λ [[x : Flonum] [p : Flonum] [Σ : Flonum]] (+ Σ (* (flexpt (- x μ) 2.0) p))) 0.0 xs ps))
  (define σ : Flonum (flexpt σ² 0.5))

  (printf "μ: ~a; Ex²: ~a; σ²: ~a; σ: ~a~n"
          (~r μ #:precision '(= 4))
          (~r Ex² #:precision '(= 4))
          (~r σ² #:precision '(= 4))
          (~r σ #:precision '(= 4))))
