#lang typed/racket/base

(provide (all-defined-out))

(define value-name : (-> Any Symbol)
  (lambda [v]
    (define name (object-name v))
    (or (and (symbol? name) name)
        (and name (string->symbol (format "<object-name:~a>" name)))
        (string->symbol (format "<object-value:~a>" v)))))
