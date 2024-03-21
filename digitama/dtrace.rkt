#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dtrace-Receiver (-> Symbol String Any Symbol Any))

(struct sentry
  ([hint : Any]
   [handler : Dtrace-Receiver]
   [terminate? : Boolean])
  #:constructor-name dtrace-sentry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-severities : (Immutable-HashTable Symbol Byte)
  (hasheq 'emergency 0
          'alert 1
          'critical 2
          'fatal 3
          'error 4
          'warning 5
          'notice 6
          'info 7
          'note 8
          'debug 9
          'trace 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dtrace
  ([level : Symbol]
   [urgent : Any])
  #:type-name Dtrace
  #:transparent)

(define dtrace-level<? : (-> Symbol Symbol Boolean)
  (lambda [lv rv]
    (< (hash-ref dtrace-severities lv (λ [] 100))
       (hash-ref dtrace-severities rv (λ [] 0)))))

(define dtrace-level-okay? : (-> Symbol Symbol Symbol Boolean)
  (lambda [listened received racket]
    (or (eq? received racket) ; already filtered by racket logging facility
        (eq? listened received)
        (dtrace-level<? received listened))))
