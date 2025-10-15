#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/string)

(require "system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tr-d : (-> Path-String String)
  (lambda [p]
    (define path (if (path? p) (path->string p) p))
    (define zdir/ (path->string (digimon-path 'zone)))
    
    ; this way also walks for cmd options like `-L/path`
    (string-replace path zdir/ "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tr-path-arguments : (case-> [(Listof Path-String) -> (Listof String)]
                                    [(Listof Any) -> (Listof Any)])
  (lambda [argl]
    (for/list ([arg (in-list argl)])
      (cond [(path? arg)   (tr-d arg)]
            [(string? arg) (tr-d arg)]
            [else arg]))))
