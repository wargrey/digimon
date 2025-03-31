#lang racket/base

(provide (all-defined-out))

(require scribble/base)
(require scribble/core)
(require scribble/decode)

(require "tag.rkt")
(require "block.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-subfigure-index-format (make-parameter "(~a)"))
(define tamer-subfigure-ref-format (make-parameter "~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subfigure-flows
  (lambda [pre-flows substyle [fmt (tamer-subfigure-index-format)] [sep #false] [order values]]
    (define subfigures (apply append (map subfigure-flatten (decode-flow pre-flows))))
    (define n (length subfigures))

    (if (> n 1)
        (list (tabular #:column-properties '(center)
                       #:sep sep
                       (let make-subfigures ([subs subfigures]
                                             [idx 97]
                                             [serugif null]
                                             [slebal null])
                         (cond [(pair? subs)
                                (let-values ([(self rest) (values (car subs) (cdr subs))])
                                  (if (paragraph? self)
                                      (let-values ([(sub caption) (subfigure-extract self idx substyle fmt)])
                                        (make-subfigures rest (add1 idx) (cons sub serugif) (cons caption slebal)))
                                      (make-subfigures rest (add1 idx) (cons self serugif) (cons null slebal))))]
                               [(andmap null? slebal) (list (reverse serugif))]
                               [else (order (list (reverse serugif) (reverse slebal)))]))))
        subfigures)))

(define subfigure-extract
  (lambda [p idx substyle fmt]
    (define self (paragraph-content p))
    (define subcap (cdr self))
    (define subidx (string (integer->char idx)))
    (define subtag (tamer-subfigure-tag (current-block-id) subidx))
    
    (values (car self)
            (if (pair? subcap)
                (centered (para #:style substyle
                                (tamer-elemtag subtag (format fmt subidx))
                                ~ subcap))
                null))))

; Yes, only flatten the top-level block
(define subfigure-flatten
  (lambda [self]
    (if (compound-paragraph? self)
        (compound-paragraph-blocks self)
        (list self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-subfigure-tag
  (lambda [parent-tag subidx]
    (format "~a:~a" parent-tag subidx)))
