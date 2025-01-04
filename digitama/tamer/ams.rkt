#lang at-exp racket/base

(provide (except-out (all-defined-out) make-equation-tag))

(require scribble/core)
(require scribble/manual)

(require racket/symbol)

(require "backend.rkt")
(require "theme.rkt")
(require "texbook.rkt")
(require "tag.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define math-inline-style (make-style "texMathInline" (list 'exact-chars)))
(define math-display-style (make-style "texMathDisplay" (list 'exact-chars)))
(define math-eqnarray-style (make-style "texMathArray" (list 'exact-chars)))
(define math-equation-style (make-style "texMathGroup" (list 'exact-chars)))

(define tamer-default-equation-label (make-parameter "Equation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $
  (lambda strs
    (make-traverse-element
     (λ [get set!]
       (if (handbook-latex-renderer? get)
           (make-element math-inline-style strs)
           (apply math strs))))))

(define $$
  (lambda strs
    (make-traverse-element
     (λ [get set!]
       (if (handbook-latex-renderer? get)
           (make-element math-display-style strs)
           (apply math strs))))))

(define $$*
  (lambda strs
    (make-traverse-element
     (λ [get set!]
       (if (handbook-latex-renderer? get)
           (make-element math-eqnarray-style strs)
           (apply math strs))))))

(define $$=
  (lambda [#:tag [tag #false] . strs]
    (if (pair? strs)
        (make-traverse-element
         (λ [get set!]
           (if (handbook-latex-renderer? get)
               (make-multiarg-element math-equation-style
                                      (if (not tag)
                                          (list (car strs) (cdr strs) "equation*" null)
                                          (list (car strs) (cdr strs) "equation" (texbook-command "label" (make-equation-tag tag)))))
               (apply math strs))))
         strs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eqref
  (lambda [tag]
    (make-traverse-element
     (λ [get set!]
       (if (handbook-latex-renderer? get)
           (:stx:link (list (format "~a" (tamer-default-equation-label))
                            (texbook-command "eqref" (make-equation-tag tag))))
           null)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; copied from the latex-render
(define make-equation-tag
  (lambda [tag]
    (apply string-append
           (for/list ([c (in-string (format "eq:~a" tag))])
             (cond [(and (or (char-alphabetic? c) (char-numeric? c)) ((char->integer c) . < . 128)) (string c)]
                   [(char=? c #\space) "_"]
                   [else (format "x~x" (char->integer c))])))))
