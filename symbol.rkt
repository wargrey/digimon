#lang typed/racket/base

(provide (all-defined-out))

(define datum-name : (-> Any Symbol)
  (lambda [v]
    (define name (object-name v))
    (cond [(symbol? name) name]
          [(and name) (string->symbol (format "<object-name:~a>" name))]
          [(symbol? v) (string->symbol (format "<symbol:~a>" v))]
          [else (string->symbol (format "<object-value:~a>" v))])))
