#lang racket/base

(provide (all-defined-out))

(require scribble/core)

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-content-filter
  (lambda [c]
    (cond [(content? c) c]
          [else (~a c)])))

(define handbook-block-filter
  (lambda [c]
    (cond [(block? c) c]
          [else (make-paragraph plain c)])))
