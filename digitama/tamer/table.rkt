#lang racket/base

(provide (all-defined-out))

(require racket/list)

(require scribble/base)
(require scribble/core)
(require scribble/decode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define table-flows
  (lambda [pre-flows]
    (define clean-flows
      (for/list ([flow (in-list pre-flows)]
                 #:when (or (block? flow) (list? flow)))
        flow))
    
    (if (and (null? (cdr clean-flows))
             (block? (car clean-flows)))
        (list (car clean-flows))
        (list (tamer-tabular/3-lines clean-flows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-tabular/3-lines
  (lambda [rows]
    (define n (length rows))
    (define borders
      (cond [(> n 1) (append '((top-border bottom-border)) (make-list (- n 2) null) '(bottom-border))]
            [(= n 1) '((top-border bottom-border))]
            [else null]))

    (tabular #:row-properties borders
             (for/list ([row (in-list rows)])
               (for/list ([col (in-list row)])
                 (if (block? col) col (centered col)))))))
