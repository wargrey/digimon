#lang typed/racket/base

(provide (all-defined-out))

(require "stone/iana/media-types.rktl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define iana-media-type-ref : (->* (Symbol) ((Option Symbol)) (Option Symbol))
  (lambda [type [category #false]]
    (if (not category)
        (for/or : (Option Symbol) ([(category types) (in-hash media-types)])
          (hash-ref types type (λ [] #false)))
        (let ([?types (hash-ref media-types category (λ [] #false))])
          (and ?types
               (hash-ref ?types type (λ [] #false)))))))
