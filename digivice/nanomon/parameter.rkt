#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name : Symbol 'nanomon)
(define the-print-width : Index 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-nanomon-shell : (Parameterof (Option Symbol)) (make-parameter #false))

(define nanomon-errno : (Parameterof Byte) (make-parameter 1))
(define nanomon-verbose : (Parameterof Boolean) (make-parameter #false))
(define nanomon-debug : (Parameterof Boolean) (make-parameter #false))
(define nanomon-silent : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-restore-options! : (-> Void)
  (lambda []
    (nanomon-errno 1)
    (nanomon-verbose #false)
    (nanomon-debug #false)
    (nanomon-silent #false)
    (pretty-print-columns the-print-width)))
