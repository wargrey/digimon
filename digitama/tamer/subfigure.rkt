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
    (define subfigures (decode-flow pre-flows))
    (define n (length subfigures))

    (if (> n 1)
        (list (tabular #:column-properties '(center)
                       #:sep sep
                       (let make-subfigures ([subs subfigures]
                                             [idx 97]
                                             [serugif null]
                                             [slebal null])
                         (cond [(pair? subs)
                                (let* ([self (paragraph-content (car subs))]
                                       [subcap (cdr self)]
                                       [subidx (string (integer->char idx))]
                                       [subtag (tamer-subfigure-tag (current-block-id) subidx)])
                                  (make-subfigures (cdr subs) (add1 idx)
                                                   (cons (car self) serugif)
                                                   (cons (if (pair? subcap)
                                                             (elem #:style substyle
                                                                   (tamer-elemtag subtag (format fmt subidx))
                                                                   ~ subcap)
                                                             null)
                                                         slebal)))]
                               [(andmap null? slebal) (list (reverse serugif))]
                               [else (order (list (reverse serugif) (reverse slebal)))]))))
        subfigures)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-subfigure-tag
  (lambda [parent-tag subidx]
    (format "~a:~a" parent-tag subidx)))
