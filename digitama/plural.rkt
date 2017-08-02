#lang typed/racket/base

(provide (all-defined-out))

(define plural : (-> Integer String String)
  (lambda [n word]
    (define dict : (HashTable String String) #hash(("story" . "stories") ("Story" . "Stories")))
    (cond [(= n 1) word]
          [else (hash-ref dict word (λ _ (string-append word "s")))])))
