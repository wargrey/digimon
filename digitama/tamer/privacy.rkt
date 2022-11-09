#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unsafe-author-info : (->* (String) (String) String)
  (lambda [env-name [fallback ""]]
    (or (getenv env-name) fallback)))
