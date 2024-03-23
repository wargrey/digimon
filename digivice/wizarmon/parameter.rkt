#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name : Symbol 'wizarmon)
(define the-print-width : Index 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-lang : (Parameterof (Option String)) (make-parameter #false))
(define wizarmon-errno : (Parameterof Byte) (make-parameter 1))
(define wizarmon-verbose : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-silent : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-restore-options! : (-> Void)
  (lambda []
    (wizarmon-lang #false)
    (wizarmon-errno 1)
    (wizarmon-verbose #false)
    (wizarmon-silent #false)
    (pretty-print-columns the-print-width)))
