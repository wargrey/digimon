#lang typed/racket/base

(provide (all-defined-out))

(define dtrace-severities : (Immutable-HashTable Symbol Byte)
  (hasheq 'emergency 0
          'alert 1
          'critical 2
          'fatal 3
          'error 4
          'warning 5
          'notice 6
          'info 7
          'debug 8
          'trace 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dtrace
  ([level : Symbol]
   [urgent : Any])
  #:type-name Dtrace
  #:transparent)

(define dtrace-level-okay? : (-> Symbol Symbol Symbol Boolean)
  (lambda [listened received racket]
    (or (eq? received racket) ; already filtered by racket logging facility
        (eq? listened received)
        (< (hash-ref dtrace-severities received (λ [] 100))
           (hash-ref dtrace-severities listened (λ [] 0))))))
