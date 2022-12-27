#lang typed/racket/base

(provide (all-defined-out))

(require "../../../symbol.rkt")
(require "../../../filesystem.rkt")

(require racket/string)

;;; https://github.com/github/linguist/tree/master/lib/linguist/languages.yml

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Github-Language-Extensions (Pairof Bytes (Listof Bytes)))
(define-type Github-Language-Misc-Datum (U String Symbol (Pairof Symbol (Listof Symbol)) (Pairof String (Listof String))))

(define px:name-label : PRegexp #px"^(\\S[^:]*):")
(define px:property-label : PRegexp #px"^\\s+(\\w+):")
(define px:array : PRegexp #px"^\\s+[-]")

(struct github-language
  ([id : Index]
   [name : String]
   [type : Symbol]
   [extensions : Github-Language-Extensions]
   [color : (Option Keyword)]
   [parent : (Option String)]
   [misc : (Immutable-HashTable Symbol Github-Language-Misc-Datum)])
  #:type-name Github-Language
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-reader read-language-metainfos #:+ (Immutable-HashTable Index Github-Language)
  (lambda [/dev/ymlin src]
    (let skip-comment ()
      (define line (read-line /dev/ymlin))

      (when (string? line)
        (cond [(regexp-match? #px"^[-]+$" line) (newline)]
              [(string-prefix? line "#") (skip-comment)]
              [else (skip-comment)])))
    
    (let language-fold ([langs : (Immutable-HashTable Index Github-Language) (hasheq)])
      (define lang : (Option Github-Language) (linguist-read-language /dev/ymlin))
      
      (cond [(not lang) langs]
            [else (language-fold (hash-set langs (github-language-id lang) lang))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define linguist-read-language : (-> Input-Port (Option Github-Language))
  (lambda [/dev/ymlin]
    (define maybe-name : (Option Bytes) (linguist-read-label /dev/ymlin px:name-label #true))
    
    (and (bytes? maybe-name)
         (let read-language ([id : Index 0]
                             [name : String (bytes->string/utf-8 maybe-name)]
                             [type : Symbol 'programming]
                             [extensions : Github-Language-Extensions (list #"")]
                             [color : (Option Keyword) #false]
                             [group : (Option String) #false]
                             [misc : (Immutable-HashTable Symbol Github-Language-Misc-Datum) (hasheq)])
           (define maybe-field : (Option Bytes)
             (let ([leading-char (peek-char /dev/ymlin)])
               (and (char? leading-char) (char-whitespace? leading-char)
                    (linguist-read-label /dev/ymlin px:property-label #false))))
           
           (cond [(not maybe-field) (github-language id name type extensions color group misc)]
                 [(bytes=? maybe-field #"language_id") (read-language (linguist-read/line* /dev/ymlin index?) name type extensions color group misc)]
                 [(bytes=? maybe-field #"type") (read-language id name (linguist-read/line* /dev/ymlin symbol?) extensions color group misc)]
                 [(bytes=? maybe-field #"color") (read-language id name type extensions (linguist-read/line* /dev/ymlin string? linguist-string->color) group misc)]
                 [(bytes=? maybe-field #"extensions") (read-language id name type (linguist-read-array /dev/ymlin string->bytes/utf-8 symbol->bytes) color group misc)]
                 [(bytes=? maybe-field #"group") (read-language id name type extensions color (string-trim (assert (read-line /dev/ymlin) string?)) misc)]
                 [else (let* ([fname (bytes->symbol maybe-field)]
                              [value (case fname
                                       [(interpreters) (linguist-read-array /dev/ymlin string->symbol (inst values Symbol Symbol))]
                                       [(aliases) (linguist-read-array /dev/ymlin)]
                                       [(ace_mode codemirror_mode codemirror_mode_type tm_scope) (linguist-read/line* /dev/ymlin symbol?)]
                                       [(filenames) (linguist-read-array /dev/ymlin (inst values String String) symbol->immutable-string)]
                                       [(wrap) (linguist-read/line* /dev/ymlin symbol?)]
                                       [(fs_name) (string-trim (assert (read-line /dev/ymlin) string?))]
                                       [else (void (read-line /dev/ymlin))])])
                         (read-language id name type extensions color group (if (void? value) misc (hash-set misc fname value))))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define linguist-read-label : (-> Input-Port PRegexp Boolean (Option Bytes))
  (lambda [/dev/ymlin px skip-line?]
    (define label (regexp-match px /dev/ymlin))

    (when (and label skip-line?)
      (read-line /dev/ymlin))
    
    (and label (cadr label))))

(define linguist-read-array : (All (a) (case-> [Input-Port (-> String a) (-> Symbol a) -> (Pairof a (Listof a))]
                                               [Input-Port -> (Pairof String (Listof String))]))
  (case-lambda
    [(/dev/ymlin string->datum symbol->datum)
     (let read-array ([array : (Listof a) null])
       (let ([array-line? (regexp-try-match px:array /dev/ymlin)])
         (cond [(not array-line?) (read-line /dev/ymlin) (assert (reverse array) pair?)]
               [else (let ([datum (read /dev/ymlin)])
                       (cond [(symbol? datum) (read-array (cons (symbol->datum datum) array))]
                             [(string? datum) (read-array (cons (string->datum datum) array))]
                             [else (read-array array)]))])))]
    [(/dev/ymlin)
     (let read-array ([array : (Listof String) null])
       (let ([array-line? (regexp-try-match px:array /dev/ymlin)])
         (cond [(not array-line?) (assert (reverse array) pair?)]
               [else (read-array (cons (string-trim (assert (read-line /dev/ymlin) string?)) array))])))]))

(define linguist-read/line* : (All (a b) (case-> [Input-Port (-> Any Boolean : a) (-> a b) -> b]
                                                 [Input-Port (-> Any Boolean : a) -> a]))
  (case-lambda
    [(/dev/ymlin predicate? datum-filter)
     (let ([datum (read /dev/ymlin)])
       (read-line /dev/ymlin)
       (datum-filter (assert datum predicate?)))]
    [(/dev/ymlin predicate?)
     ((inst linguist-read/line* a a) /dev/ymlin predicate? values)]))

(define linguist-string->color : (-> String Keyword)
  (lambda [strcolor]
    (string->keyword (substring strcolor 1))))
