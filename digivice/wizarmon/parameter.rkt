#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name : Symbol 'wizarmon)
(define the-print-width : Index 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-timeout : (Parameterof Natural) (make-parameter 0))
(define wizarmon-strict : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-stdio-line-limit : (Parameterof Natural) (make-parameter 32))

(define wizarmon-lang : (Parameterof (Option String)) (make-parameter #false))
(define wizarmon-errno : (Parameterof Byte) (make-parameter 1))
(define wizarmon-verbose : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-silent : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-remake : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-restore-options! : (-> Void)
  (lambda []
    (wizarmon-lang #false)
    (wizarmon-strict #false)
    (wizarmon-errno 1)
    (wizarmon-verbose #false)
    (wizarmon-silent #false)
    (wizarmon-remake #false)
    (pretty-print-columns the-print-width)))
