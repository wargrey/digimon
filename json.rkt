#lang typed/racket/base

(provide (all-defined-out))
(provide JSON-Stdin JSON-Datum JSExpr)

(require "digitama/json.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define json-datum? : (-> Any Boolean : JSON-Datum)
  (lambda [val]
    (or (string? val)
        (boolean? val)
        (exact-integer? val)
        (flonum? val)
        (void? val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-json : (-> JSON-Stdin JSExpr)
  (lambda [/dev/stdin]
    (json-extract-value (json-stdin->port /dev/stdin))))
