#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-print-width : Index 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-lang : (Parameterof (Option String)) (make-parameter #false))
(define wizarmon-skip-view : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-errno : (Parameterof Byte) (make-parameter 1))
(define wizarmon-verbose : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-debug : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-silent : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-remake : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-restore-options! : (-> Void)
  (lambda []
    (wizarmon-errno 1)
    (wizarmon-lang #false)
    (wizarmon-skip-view #false)
    (wizarmon-verbose #false)
    (wizarmon-debug #false)
    (wizarmon-silent #false)
    (wizarmon-remake #false)
    (pretty-print-columns the-print-width)))
