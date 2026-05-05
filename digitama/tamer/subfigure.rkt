#lang racket/base

(provide (all-defined-out))

(require racket/match)

(require scribble/base)
(require scribble/core)
(require scribble/decode)

(require "tag.rkt")
(require "block.rkt")
(require "backend.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-subfigure-index-format (make-parameter "(~a)"))
(define tamer-subfigure-ref-format (make-parameter "~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subfigure-flows
  (lambda [pre-flows legend-style [fmt (tamer-subfigure-index-format)] [pad:ex 0.0] [sub-align 'bottom] [sub-label-align 'top] [order values] [sub-tabular? #false]]
    (define subfigures (decode-flow pre-flows))
    (define block-who (current-block-id))
    (define-values (hpad vpad)
      (match pad:ex
        [(? real?) (values (list pad:ex 0.0 pad:ex 0.0) (list 0.0 pad:ex 0.00 pad:ex))]
        [(list h v) (values (list h 0.0 h 0.0) (list 0.0 v 0.0 v))]
        [(list l t r b) (values (list l 0.0 r 0.0) (list 0.0 t 0.0 b))]
        [(cons h v) (values (list h 0.0 h 0.0) (list 0.0 v 0.0 v))]
        [else (values 0.0 0.0)]))

    (cond [(null? subfigures) null]
          [(and (null? (cdr subfigures)) (not (compound-paragraph? (car subfigures)))) subfigures]
          [(and sub-tabular?) (list (tabular-subfigures subfigures legend-style fmt hpad vpad sub-align sub-label-align order))]
          [else (list (make-traverse-block
                       (λ [get set!]
                         (parameterize ([current-block-id block-who])
                           (if (handbook-latex-renderer? get)
                               (tabular-subfigures subfigures legend-style fmt hpad vpad sub-align sub-label-align order)
                               (tabular-subfigures subfigures legend-style fmt hpad vpad sub-align sub-label-align order))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tabular-subfigures
  (lambda [src legend-style fmt hpad vpad sub-align sub-label-align order]
    (let subfigure ([subfigures src]
                    [idx 97]
                    [swor null])
      (if (pair? subfigures)
          (let-values ([(self rest) (values (car subfigures) (cdr subfigures))])
            (cond [(paragraph? self)
                   (let ([cell (subfigure-single-column self legend-style fmt order idx sub-align sub-label-align)])
                     (subfigure rest (+ idx 1) (cons (list cell) swor)))]
                  [(compound-paragraph? self)
                   (let-values ([(cell idx++) (subfigure-columns self legend-style fmt order idx sub-align sub-label-align hpad)])
                     (subfigure rest idx++ (cons (list cell) swor)))]
                  [else (subfigure rest idx swor)]))

          (tabular #:column-properties '(center)
                   #:pad vpad
                   (reverse swor))))))

(define subfigure-single-column
  (lambda [self legend-style fmt order idx sub-align sub-label-align]
    (define-values (figure tag caption) (subfigure-extract self idx fmt))
    
    (if (and caption)
        (let ([subrows (order (list figure (subfigure-legend legend-style tag caption)))])
          (tabular #:column-properties '(center)
                   #:row-properties (order (list sub-align sub-label-align))
                   (list (list (car subrows))
                         (list (cadr subrows)))))
        (tabular #:column-properties '(center)
                 #:row-properties (list sub-align)
                 (list (list figure))))))

(define subfigure-columns
  (lambda [self legend-style fmt order cpt-idx sub-align sub-label-align hpad]
    ;;; TODO: what if the subfigure block is a delayed block
    (define-values (cells aligns idx++)
      (let make-subfigures ([subfigures (filter paragraph? (compound-paragraph-blocks self))]
                            [idx cpt-idx]
                            [serugif null]
                            [slebal null])
        (cond [(pair? subfigures)
               (let*-values ([(self rest) (values (car subfigures) (cdr subfigures))]
                             [(figure tag caption) (subfigure-extract self idx fmt)])
                 (cond [(and figure)
                        (make-subfigures rest (add1 idx)
                                         (cons (para #:style subfigureinside-style figure) serugif)
                                         (cons (subfigure-legend legend-style tag caption) slebal))]
                       [else (make-subfigures rest idx (cons phantom-cell serugif) (cons phantom-cell slebal))]))]
              [(andmap subfigure-phantom? slebal) (values (list (reverse serugif)) (list sub-align) idx)]
              [else (values (order (list (reverse serugif) (reverse slebal)))
                            (order (list sub-align sub-label-align))
                            idx)])))

    (values (tabular #:column-properties '(center)
                     #:row-properties aligns
                     #:pad hpad
                     cells)
            idx++)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subfigure-extract
  (lambda [p idx fmt]
    (define self (paragraph-content p))

    (if (pair? self)
        (let* ([subcap (cdr self)]
               [subidx (string (integer->char idx))]
               [subtag (tamer-subfigure-tag (current-block-id) subidx)])
          (values (car self)
                  (tamer-elemtag subtag (format fmt subidx))
                  (and (pair? subcap) subcap)))
        (values #false #false #false))))

(define subfigure-legend
  (lambda [legend-style tag caption]
    ;;; The `para` with a string-named style is non-boxable,
    ;;;   so the `tabular` boxes it (via the `minipage`),
    ;;;   ensuring fixed column widths
    (if (and caption)
        (para #:style legend-style tag ~ caption)
        phantom-cell)))

(define subfigure-phantom?
  (lambda [v]
    (eq? v phantom-cell)))

(define tamer-subfigure-tag
  (lambda [parent-tag subidx]
    (format "~a:~a" parent-tag subidx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define phantom-cell (para #:style tamer-figure-sublegend-style ~))
(define subfigureinside-style (make-style "SubFigureInside" block-style-extras))
