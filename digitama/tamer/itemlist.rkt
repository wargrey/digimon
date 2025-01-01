#lang racket/base

(provide (all-defined-out))
(provide (rename-out [tamer-itemlist handbook-itemlist]))

(require scribble/core)
(require scribble/manual)

(require "backend.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-itemlist
  (lambda [#:style [style #false] . items]
    (make-traverse-block
     (λ [get set!]
       (if (handbook-latex-renderer? get)
           (cond [(eq? style 'compact) (apply itemlist #:style "HandbookCompactItemize" items)]
                 [(eq? style 'ordered) (apply itemlist #:style "HandbookCompactOrdered" items)]
                 [else (apply itemlist #:style style items)])
           (apply itemlist #:style style items))))))
