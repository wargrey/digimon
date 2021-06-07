#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/symbol))
(provide (all-from-out racket/keyword))

(require racket/symbol)
(require racket/keyword)
(require racket/string)

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

(define symbol-list? : (-> Any Boolean : (Listof Symbol))
  (lambda [a]
    (and (list? a)
         (andmap symbol? a))))

(define keyword-list? : (-> Any Boolean : (Listof Keyword))
  (lambda [a]
    (and (list? a)
         (andmap keyword? a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol->keyword : (-> Symbol Keyword)
  (lambda [s]
    (string->keyword (symbol->immutable-string s))))

(define keyword->symbol : (-> Keyword Symbol)
  (lambda [k]
    (string->symbol (keyword->immutable-string k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol-join : (->* ((Listof Symbol)) (String #:before-first String #:before-last String #:after-last String) String)
  (lambda [symbols [sep " "] #:before-first [bf ""] #:before-last [bl sep] #:after-last [al ""]]
    (string-join #:before-first bf #:before-last bl #:after-last al
                 (map symbol->immutable-string symbols) sep)))

(define keyword-join : (->* ((Listof Keyword)) (String #:before-first String #:before-last String #:after-last String) String)
  (lambda [symbols [sep " "] #:before-first [bf ""] #:before-last [bl sep] #:after-last [al ""]]
    (string-join #:before-first bf #:before-last bl #:after-last al
                 (map keyword->immutable-string symbols) sep)))
