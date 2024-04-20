#lang racket/base

(provide (all-defined-out))

(require racket/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-stat-render-name 'handbook-stat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-renderer?
  (lambda [get render]
    (and (memq render
               (cond [(procedure? get) (get 'scribble:current-render-mode null)]
                     [else (send get current-render-mode)]))
         #true)))

(define handbook-latex-renderer?
  (lambda [get]
    (handbook-renderer? get 'latex)))

(define handbook-markdown-renderer?
  (lambda [get]
    (handbook-renderer? get 'markdown)))

(define handbook-stat-renderer?
  (lambda [get]
    (handbook-renderer? get handbook-stat-render-name)))
