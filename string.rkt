#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/string))
(provide (all-from-out "digitama/unicode.rkt"))

(require racket/string)
(require racket/list)

(require "digitama/unicode.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-uri? : (-> (U String Bytes) Boolean)
  (let ([rx:3986 #rx"^(?:([^:/?#]*):)?(?://(?:([^/?#@]*)@)?(?:(?:\\[([0-9a-fA-F:]*:[0-9a-fA-F:]*)\\])|([^/?#:]*))?(?::([0-9]*))?)?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$"])
    (lambda [s]
      (regexp-match? rx:3986 s))))

(define string-guid? : (-> (U String Bytes) Boolean)
  (let ([px:guid #px"^[{][0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}[}]$"])
    (lambda [s]
      (regexp-match? px:guid s))))

(define string-panose? : (-> (U String Bytes) Boolean) ; a classification system for Font
  (let ([px:panose #px"^[0-9A-F]{10}$"])
    (lambda [s]
      (regexp-match? px:panose s))))

(define <<string>>? : (-> (U String Bytes) Boolean)
  (let ([px:<<w+>> #px"^&lt;&lt;\\w+&gt;&gt;$"])
    (lambda [s]
      (regexp-match? px:<<w+>> s))))

(define <<string>>+? : (-> (Listof (U String Bytes)) Boolean)
  (lambda [s]
    (ormap <<string>>? s)))

(define string-match-any? : (-> (Listof (U String Bytes)) (U Byte-Regexp Regexp) Boolean)
  (lambda [ss patterns]
    (for/or : Boolean ([label (in-list ss)])
      (regexp-match? patterns label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-empty? : (-> String Boolean)
  (lambda [s]
    (zero? (string-length s))))

(define string-blank? : (-> String Boolean)
  (lambda [s]
    (or (zero? (string-length s))
        (regexp-match? #px"^\\s+$" s))))

(define string!empty? : (-> String Boolean)
  (lambda [s]
    (not (string-empty? s))))

(define string!blank? : (-> String Boolean)
  (lambda [s]
    (not (string-blank? s))))

(define string-filter : (-> String (Option String))
  (lambda [s]
    (and (string!blank? s)
         s)))

(define string-list-trim-blanks : (->* ((Listof String)) ((-> String Boolean)) (Listof String))
  (lambda [strs [blank? string-blank?]]
    (dropf-right (dropf strs blank?) blank?)))

(define string-list-normalize-blanks : (->* ((Listof String)) ((-> String Boolean)) (Listof String))
  (lambda [strs [blank? string-blank?]]
    (let normalize ([src : (Listof String) strs]
                    [srts : (Listof String) null]
                    [last-empty? : Boolean #true])
      (if (pair? src)
          (let*-values ([(self tail) (values (car src) (cdr src))]
                        [(size) (string-length self)])
            (cond [(blank? self) (normalize tail srts #true)]
                  [(not last-empty?) (normalize tail (cons self srts) #false)]
                  [(null? srts) (normalize tail (cons self srts) #false)]
                  [else (normalize tail (cons self (cons "" srts)) #false)]))
          (reverse srts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define locale-bytes->unicode-string : (->* (Bytes (U String Symbol False)) (Natural Natural #:error-char (Option Char)) String)
  (lambda [raw lc-all [start 0] [end (bytes-length raw)] #:error-char [echar #false]]
    (case lc-all
      [(#false utf-8 UTF-8) (bytes->string/utf-8 raw echar start end)]
      [(locale) (bytes->string/locale raw echar start end)]
      [(latin-1) (bytes->string/latin-1 raw echar start end)]
      [else (let ([->utf-8 (bytes-open-converter (if (symbol? lc-all) (symbol->string lc-all) lc-all) "UTF-8")])
              (cond [(not ->utf-8) (bytes->string/locale raw echar start end)]
                    [else (let-values ([(raw/utf-8 n status) (bytes-convert ->utf-8 raw)])
                            (bytes-close-converter ->utf-8)
                            (bytes->string/utf-8 raw/utf-8 echar))]))])))

(define unicode-string->locale-bytes : (->* (String (U String Symbol False)) (Natural Natural #:error-byte (Option Byte)) Bytes)
  (lambda [raw lc-all [start 0] [end (string-length raw)] #:error-byte [ebyte #false]]
    (case lc-all
      [(#false utf-8 UTF-8) (string->bytes/utf-8 raw ebyte start end)]
      [(locale) (string->bytes/locale raw ebyte start end)]
      [(latin-1) (string->bytes/latin-1 raw ebyte start end)]
      [else (let ([->locale (bytes-open-converter "UTF-8" (if (symbol? lc-all) (symbol->string lc-all) lc-all))])
              (cond [(not ->locale) (string->bytes/locale raw ebyte start end)]
                    [else (let-values ([(raw/locale n status) (bytes-convert ->locale (string->bytes/utf-8 raw ebyte start end))])
                            (bytes-close-converter ->locale)
                            raw/locale)]))])))

(define locale-bytes-unicode-length : (->* (Bytes (U String Symbol False)) (Natural Natural #:error-char (Option Char)) Index)
  (lambda [raw lc-all [start 0] [end (bytes-length raw)] #:error-char [echar #false]]
    (case lc-all
      [(#false utf-8 UTF-8) (bytes-utf-8-length raw echar start end)]
      [(locale) (string-length (bytes->string/locale raw echar start end))]
      [(latin-1) (string-length (bytes->string/latin-1 raw echar start end))]
      [else (let ([->utf-8 (bytes-open-converter (if (symbol? lc-all) (symbol->string lc-all) lc-all) "UTF-8")])
              (cond [(not ->utf-8) (string-length (bytes->string/utf-8 raw echar start end))]
                    [else (let-values ([(raw/utf-8 n status) (bytes-convert ->utf-8 raw)])
                            (bytes-close-converter ->utf-8)
                            (bytes-utf-8-length raw/utf-8))]))])))

(define unicode-string-locale-length : (->* (String (U String Symbol False)) (Natural Natural #:error-byte (Option Byte)) Index)
  (lambda [raw lc-all [start 0] [end (string-length raw)] #:error-byte [ebyte #false]]
    (case lc-all
      [(#false utf-8 UTF-8) (string-utf-8-length raw start end)]
      [(locale) (bytes-length (string->bytes/locale raw ebyte start end))]
      [(latin-1) (bytes-length (string->bytes/latin-1 raw ebyte start end))]
      [else (let ([->locale (bytes-open-converter "UTF-8" (if (symbol? lc-all) (symbol->string lc-all) lc-all))])
              (cond [(not ->locale) (bytes-length (string->bytes/locale raw ebyte start end))]
                    [else (let-values ([(raw/locale n status) (bytes-convert ->locale (string->bytes/utf-8 raw ebyte start end))])
                            (bytes-close-converter ->locale)
                            (bytes-length raw/locale))]))])))
