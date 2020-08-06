#lang racket/base

(provide (all-defined-out))

(require racket/symbol)

(require scribble/manual)
(require scribble/core)
(require scribble/decode)
(require scribble/html-properties)
(require scribble/latex-properties)
(require setup/main-collects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-figure-index-type 'digimon:tamer:figure)

(define tamer-default-figure-label (make-parameter "Figure"))
(define tamer-default-figure-label-separator (make-parameter ": "))
(define tamer-default-figure-label-style (make-parameter 'tt))
(define tamer-default-figure-caption-style (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-figure-block
  (lambda [content-style chpt-idx self-idx tag caption content]
    (define label (format "~a ~a.~a~a" (tamer-default-figure-label) chpt-idx self-idx (tamer-default-figure-label-separator)))
    (define legend
      (list (elemtag (symbol->immutable-string tag) (make-element (tamer-default-figure-label-style) label))
            (make-element (tamer-default-figure-caption-style) caption)))
    
    (list (make-nested-flow content-style (list (make-nested-flow figureinside-style (decode-flow content))))
          (make-paragraph centertext-style (list (make-element legend-style legend))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list 'never-indents
          (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))
(define figuremulti-style (make-style "FigureMulti" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define legend-style (make-style "Legend" figure-style-extras))
(define centertext-style (make-style "Centertext" figure-style-extras))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API layer
(define tamer-center-figure-style (make-style "Centerfigure" figure-style-extras))
(define tamer-left-figure-style (make-style "Leftfigure" figure-style-extras))
(define tamer-right-figure-style (make-style "Rightfigure" figure-style-extras))
