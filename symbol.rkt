#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/symbol))
(provide (all-from-out racket/keyword))

(require racket/symbol)
(require racket/keyword)
(require racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define datum-name : (-> Any [#:no-prefix-for-symbol-datum? Boolean] Symbol)
  (lambda [v #:no-prefix-for-symbol-datum? [no-prefix? #false]]
    (define name (object-name v))
    (cond [(symbol? name) name]
          [(and name) (string->symbol (format "<object-name:~a>" name))]
          [(symbol? v) (if (not no-prefix?) (string->symbol (format "<symbol:~a>" v)) v)]
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

(define symbol-list+? : (-> Any Boolean : (Pairof Symbol (Listof Symbol)))
  (lambda [a]
    (and (list? a)
         (pair? a)
         (andmap symbol? a))))

(define keyword-list? : (-> Any Boolean : (Listof Keyword))
  (lambda [a]
    (and (list? a)
         (andmap keyword? a))))

(define keyword-list+? : (-> Any Boolean : (Pairof Keyword (Listof Keyword)))
  (lambda [a]
    (and (list? a)
         (pair? a)
         (andmap keyword? a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol-upcase : (-> Symbol Symbol)
  (lambda [s]
    (string->symbol (string-upcase (symbol->immutable-string s)))))

(define symbol-downcase : (-> Symbol Symbol)
  (lambda [s]
    (string->symbol (string-downcase (symbol->immutable-string s)))))

(define symbol-titlecase : (-> Symbol Symbol)
  (lambda [s]
    (string->symbol (string-titlecase (symbol->immutable-string s)))))

(define symbol-foldcase : (-> Symbol Symbol)
  (lambda [s]
    (string->symbol (string-foldcase (symbol->immutable-string s)))))

(define keyword-upcase : (-> Keyword Keyword)
  (lambda [s]
    (string->keyword (string-upcase (keyword->immutable-string s)))))

(define keyword-downcase : (-> Keyword Keyword)
  (lambda [s]
    (string->keyword (string-downcase (keyword->immutable-string s)))))

(define keyword-titlecase : (-> Keyword Keyword)
  (lambda [s]
    (string->keyword (string-titlecase (keyword->immutable-string s)))))

(define keyword-foldcase : (-> Keyword Keyword)
  (lambda [s]
    (string->keyword (string-foldcase (keyword->immutable-string s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol->keyword : (-> Symbol Keyword)
  (lambda [s]
    (string->keyword (symbol->immutable-string s))))

(define symbol->unreadable-symbol : (-> Symbol Symbol)
  (lambda [s]
    (cond [(symbol-unreadable? s) s]
          [else (string->unreadable-symbol (symbol->immutable-string s))])))

(define symbol->interned-symbol : (-> Symbol Symbol)
  (lambda [s]
    (cond [(symbol-interned? s) s]
          [else (string->symbol (symbol->immutable-string s))])))

(define keyword->symbol : (-> Keyword Symbol)
  (lambda [k]
    (string->symbol (keyword->immutable-string k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol->bytes : (-> Symbol Bytes)
  (lambda [s]
    (string->bytes/utf-8 (symbol->immutable-string s))))

(define keyword->bytes : (-> Keyword Bytes)
  (lambda [s]
    (string->bytes/utf-8 (keyword->immutable-string s))))

(define bytes->symbol : (-> Bytes Symbol)
  (lambda [s]
    (string->symbol (bytes->string/utf-8 s))))

(define bytes->keyword : (-> Bytes Keyword)
  (lambda [s]
    (string->keyword (bytes->string/utf-8 s))))

(define symbol->path : (-> Symbol Path)
  (lambda [s]
    (string->path (symbol->immutable-string s))))

(define keyword->path : (-> Keyword Path)
  (lambda [s]
    (string->path (keyword->immutable-string s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol-join : (->* ((Listof Symbol)) (String #:before-first String #:before-last String #:after-last String) String)
  (lambda [symbols [sep " "] #:before-first [bf ""] #:before-last [bl sep] #:after-last [al ""]]
    (string-join #:before-first bf #:before-last bl #:after-last al
                 (map symbol->immutable-string symbols) sep)))

(define keyword-join : (->* ((Listof Keyword)) (String #:before-first String #:before-last String #:after-last String) String)
  (lambda [symbols [sep " "] #:before-first [bf ""] #:before-last [bl sep] #:after-last [al ""]]
    (string-join #:before-first bf #:before-last bl #:after-last al
                 (map keyword->immutable-string symbols) sep)))
