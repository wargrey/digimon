#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name 'nanomon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-nanomon-shell : (Parameterof (Option Symbol)) (make-parameter #false))

(define nanomon-lang : (Parameterof (Option String)) (make-parameter #false))
(define nanomon-errno : (Parameterof Byte) (make-parameter 1))
(define nanomon-verbose : (Parameterof Boolean) (make-parameter #false))
(define nanomon-silent : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-restore-options! : (-> Void)
  (lambda []
    (nanomon-lang #false)
    (nanomon-errno 1)
    (nanomon-verbose #false)
    (nanomon-silent #false)))
