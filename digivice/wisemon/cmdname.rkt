#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(unsafe-require/typed/provide
 raco/command-name
 [current-command-name (Parameterof (Option String))]
 [short-program+command-name (-> String)]
 [program+command-name (-> String)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-cmd-name : (-> Symbol)
  (lambda []
    (string->symbol (assert (current-command-name)))))
