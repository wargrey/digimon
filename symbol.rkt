#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/symbol))
(provide (all-from-out racket/keyword))

(require racket/symbol)
(require racket/keyword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define datum-name : (-> Any Symbol)
  (lambda [v]
    (define name (object-name v))
    (cond [(symbol? name) name]
          [(and name) (string->symbol (format "<object-name:~a>" name))]
          [(symbol? v) (string->symbol (format "<symbol:~a>" v))]
          [else (string->symbol (format "<object-value:~a>" v))])))

(define list->symbol : (-> (Listof Char) Symbol)
  (lambda [lst]
    (string->symbol (list->string lst))))

(define rlist->symbol : (-> (Listof Char) Symbol)
  (lambda [lst]
    (string->symbol (list->string (reverse lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol->keyword : (-> Symbol Keyword)
  (lambda [s]
    (string->keyword (symbol->immutable-string s))))

(define keyword->symbol : (-> Keyword Symbol)
  (lambda [k]
    (string->symbol (keyword->immutable-string k))))
