#lang racket/base

(provide (all-defined-out))

(require scribble/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-content-filter
  (lambda [c]
    (cond [(content? c) c]
          [else (format "~a" c)])))

(define handbook-block-filter
  (lambda [c]
    (cond [(block? c) c]
          [else (make-paragraph plain c)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-newline-element?
  (lambda [v]
    (and (element? v)
         (eq? (element-style v)
              'newline))))
