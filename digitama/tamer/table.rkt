#lang racket/base

(provide (all-defined-out))

(require racket/list)

(require scribble/base)
(require scribble/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define table-flows
  (lambda [pre-flows [pad:ex 0]]
    (define clean-flows
      (for/list ([flow (in-list pre-flows)]
                 #:when (or (block? flow) (list? flow)))
        flow))
    
    (if (and (null? (cdr clean-flows))
             (block? (car clean-flows)))
        (list (car clean-flows))
        (list (tamer-tabular/3-lines clean-flows pad:ex)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-tabular/3-lines
  (lambda [rows [pad:ex 0]]
    (define n (length rows))
    (define borders
      (cond [(> n 1) (append '((top-border bottom-border)) (make-list (- n 2) null) '(bottom-border))]
            [(= n 1) '((top-border bottom-border))]
            [else null]))

    (tabular #:row-properties borders
             #:pad (cond [(real? pad:ex) (list pad:ex 0)]
                         [(pair? pad:ex) (list (car pad:ex) (cdr pad:ex))]
                         [else pad:ex])
             (for/list ([row (in-list rows)])
               (for/list ([col (in-list row)])
                 (cond [(block? col) col]
                       [else (centered col)]))))))
