#lang typed/racket/base

(provide (all-defined-out))

;;; https://drafts.csswg.org/css-values/#absolute-lengths
;;; https://drafts.csswg.org/css-values/#relative-lengths
;;; https://drafts.csswg.org/css-egg/#astro-units
;;; https://drafts.csswg.org/css-egg/#traditional-time

(require racket/math)

(require racket/symbol)
(require racket/flonum)

(require "struct.rkt")
(require "function.rkt")
(require "syntax.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dimension stx)
  (syntax-parse stx #:literals [:]
    [(_ dim #:=> canonical-unit
        (~optional (~seq #:with [[argv : Argv] ...])
                   #:defaults ([(argv 1) null] [(Argv 1) null]))
        [[units expr ...] ...])
     (with-syntax ([dim-units (make-identifier #'dim "~a-units")]
                   [dim-canonical-unit (make-identifier #'dim "~a-canonical-unit")]
                   [(name ...) (for/fold ([names null]) ([unit (in-syntax #'(units ...))]) (append names (syntax->datum unit)))])
       (syntax/loc stx
         (begin (define dim-units : (Listof Symbol) (list 'name ...))
                (define dim-canonical-unit : Symbol 'canonical-unit)
                
                (define dim : (case-> [Nonnegative-Flonum Symbol Argv ... -> Nonnegative-Flonum]
                                      [Flonum Symbol Argv ... -> Flonum]
                                      [(U String Symbol) Argv ... -> Flonum])
                  (case-lambda
                    [(canonical-unit unit argv ...)
                     (case unit
                       [(canonical-unit) canonical-unit]
                       [units expr ...] ...
                       [else (if (eq? unit '||) canonical-unit +nan.0)])]
                    [(literal argv ...)
                     (let-values ([(number unit) (string->dimension literal #:ci? #true)])
                       (dim number unit argv ...))])))))]))

(define-syntax (define-dimensions stx)
  (syntax-case stx [:]
    [(_ ([dim defs ...] ...))
     (syntax/loc stx
       (begin (define-dimension dim defs ...)
              ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dimension-environment : Dimension-Environment
  ([em : (-> Nonnegative-Flonum) λnan]
   [ex : (-> Nonnegative-Flonum) λnan]
   [cap : (-> Nonnegative-Flonum) λnan]
   [ch : (-> Nonnegative-Flonum) λnan]
   [ic : (-> Nonnegative-Flonum) λnan]
   [lh : (-> Nonnegative-Flonum) λnan]
   [rem : (-> Nonnegative-Flonum) λnan]
   [rlh : (-> Nonnegative-Flonum) λnan]
   [vw : (-> Nonnegative-Flonum) λnan]
   [vh : (-> Nonnegative-Flonum) λnan])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dimensions
  ([dim:length    #:=> px #:with [[denv : Dimension-Environment]]
                  [[(cm)       (fl* (fl/ 96.0 2.54) px)]
                   [(mm)       (fl* (fl/ 96.0 25.4) px)]                    #;1cm/10
                   [(q)        (fl* (fl/ 96.0 101.6) px)]                   #;1cm/40
                   [(in)       (fl* 96.0 px)]
                   [(pc)       (fl* 16.0 px)]                               #;1in/6
                   [(pt)       (fl* (fl/ 96.0 72.0) px)]                    #;1in/72
                   [(em)       (fl* ((dimension-environment-em denv)) px)]
                   [(ex)       (fl* ((dimension-environment-em denv)) px)]
                   [(cap)      (fl* ((dimension-environment-em denv)) px)]
                   [(ch)       (fl* ((dimension-environment-em denv)) px)]
                   [(ic)       (fl* ((dimension-environment-em denv)) px)]
                   [(lh)       (fl* ((dimension-environment-em denv)) px)]
                   [(rem)      (fl* ((dimension-environment-em denv)) px)]
                   [(rlh)      (fl* ((dimension-environment-em denv)) px)]
                   [(vw vi)    (fl* (fl* 0.01 ((dimension-environment-vw denv))) px)]
                   [(vh vb)    (fl* (fl* 0.01 ((dimension-environment-vh denv))) px)]
                   [(vmin)     (fl* (fl* 0.01 (min ((dimension-environment-vw denv)) ((dimension-environment-vh denv)))) px)]
                   [(vmax)     (fl* (fl* 0.01 (max ((dimension-environment-vw denv)) ((dimension-environment-vh denv)))) px)]
                   [(apc)      (fl* (fl* (fl/ 96.0 2.54) 3.086) px)]        #;3.086cm
                   [(pls)      (fl* 1.133 px)]
                   [(ls)       (fl* 1.133e12 px)]                           #;1e12pls
                   #;[(pc)       (fl* (fl* (fl/ 96.0 2.54) 3.086e18) px)]   #;1e18apc]]
   ;;; https://drafts.csswg.org/css-values/#angles
   [dim:angle      #:=> deg
                   [[(grad)    (fl* 0.9 deg)]
                    [(rad)     (fl* (fl/ 180.0 pi) deg)]
                    [(turn)    (fl* 360.0 deg)]]]
   ;;; https://drafts.csswg.org/css-values/#time
   [dim:time       #:=> s
                   [[(ms)      (fl* 0.001 s)]
                    [(min)     (fl* 60.0 s)]
                    [(h)       (fl* 3600.0 s)]
                    [(ft)      (fl* 1.2096e6 s)]
                    [(mft)     (fl* 1.2096e3 s)]]]
   ;;; https://drafts.csswg.org/css-values/#frequency
   [dim:frequency  #:=> Hz
                   [[(hz)      Hz]
                    [(kHz khz) (fl* 0.001 Hz)]]]
   ;;; https://drafts.csswg.org/css-values/#resolution
   [dim:resolution #:=> dppx
                   [[(dpcm)    (fl* (fl/ 2.54 96.0) dppx)]
                    [(dpi)     (fl* (fl/ 1.0 96.0) dppx)]
                    [(x)       dppx]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string->dimension : (-> (U String Symbol) [#:ci? Boolean] (Values Flonum Symbol))
  (lambda [literal #:ci? [ci? #false]]
    (define dim : String (if (string? literal) literal (symbol->immutable-string literal)))
    (define size : Index (string-length dim))
    
    (cond [(= size 0) (values +nan.0 '||)]
          [else (let dim-split ([idx : Index (- size 1)])
                  (define uch : Char (string-ref dim idx))

                  (cond [(char-numeric? uch)
                         (let* ([idx+1 (+ idx 1)]
                                [n (string->number (substring dim 0 idx+1))])
                           (values (if (real? n) (real->double-flonum n) +nan.0)
                                   (let ([u (substring dim idx+1 size)])
                                     (string->symbol (if (not ci?) u (string-downcase u))))))]
                        [(= idx 0) (values 1.0 (string->symbol (if (not ci?) dim (string-downcase dim))))]
                        [else (dim-split (- idx 1))]))])))
