#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define datum-name : (-> Any [#:no-prefix-for-symbol-datum? Boolean] Symbol)
  (lambda [v #:no-prefix-for-symbol-datum? [no-prefix? #false]]
    (define name (object-name v))
    (cond [(symbol? name) name]
          [(and name) (string->symbol (format "<object-name:~a>" name))]
          [(symbol? v) (if (not no-prefix?) (string->symbol (format "<symbol:~a>" v)) v)]
          [else (string->symbol (format "<object-value:~a>" v))])))
