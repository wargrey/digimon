#lang typed/racket/base

(provide main)
(provide Github-Language Language-Misc-Datum)

(require digimon/cmdopt)
(require digimon/symbol)

(require racket/string)
(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option linguist-flags #: Linguist-Flags
  #:program 'linguist
  #:args [rootdir.git]

  #:once-each
  [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Language-Misc-Datum (U String Symbol (Pairof Symbol (Listof Symbol)) (Pairof String (Listof String))))
(define-type Github-Language (List Natural String Symbol (Pairof Bytes (Listof Bytes)) (Option Keyword) (Immutable-HashTable Symbol Language-Misc-Datum)))
(define-type Github-Languages (Listof Github-Language))

(define px:name-label : PRegexp #px"^(\\S[^:]*):")
(define px:property-label : PRegexp #px"^\\s+(\\w+):")
(define px:array : PRegexp #px"^\\s+[-]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define linguist-read-language : (-> Input-Port (Option Github-Language))
  (lambda [/dev/ymlin]
    (define maybe-name : (Option Bytes) (linguist-read-label /dev/ymlin px:name-label #true))
    
    (and (bytes? maybe-name)
         (let read-language ([id : Index 0]
                             [name : String (bytes->string/utf-8 maybe-name)]
                             [type : Symbol 'programming]
                             [extensions : (Pairof Bytes (Listof Bytes)) (list #"")]
                             [color : (Option Keyword) #false]
                             [misc : (Immutable-HashTable Symbol Language-Misc-Datum) (hasheq)])
           (define maybe-field : (Option Bytes)
             (let ([leading-char (peek-char /dev/ymlin)])
               (and (char? leading-char) (char-whitespace? leading-char)
                    (linguist-read-label /dev/ymlin px:property-label #false))))
           
           (cond [(not maybe-field) (list id name type extensions color misc)]
                 [(bytes=? maybe-field #"language_id") (read-language (linguist-read/line* /dev/ymlin index?) name type extensions color misc)]
                 [(bytes=? maybe-field #"type") (read-language id name (linguist-read/line* /dev/ymlin symbol?) extensions color misc)]
                 [(bytes=? maybe-field #"color") (read-language id name type extensions (linguist-read/line* /dev/ymlin string? linguist-string->color) misc)]
                 [(bytes=? maybe-field #"extensions") (read-language id name type (linguist-read-array /dev/ymlin string->bytes/utf-8 symbol->bytes) color misc)]
                 [else (let* ([fname (bytes->symbol maybe-field)]
                              [value (case fname
                                       [(interpreters) (linguist-read-array /dev/ymlin string->symbol (inst values Symbol Symbol))]
                                       [(aliases) (linguist-read-array /dev/ymlin)]
                                       [(ace_mode codemirror_mode codemirror_mode_type tm_scope) (linguist-read/line* /dev/ymlin symbol?)]
                                       [(filenames) (linguist-read-array /dev/ymlin (inst values String String) symbol->immutable-string)]
                                       [(wrap) (linguist-read/line* /dev/ymlin symbol?)]
                                       [(group fs_name) (assert (read-line /dev/ymlin) string?)]
                                       [else (void (read-line /dev/ymlin))])])
                         (read-language id name type extensions color (if (void? value) misc (hash-set misc fname value))))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define linguist-read-label : (-> Input-Port PRegexp Boolean (Option Bytes))
  (lambda [/dev/ymlin px skip-line?]
    (define label (regexp-match px /dev/ymlin))

    (when (and skip-line?)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define linguist-dump : (-> String Void)
  (lambda [rootdir]
    (call-with-input-file* (build-path rootdir "lib" "linguist" "languages.yml")
      (λ [[/dev/ymlin : Input-Port]] : Void
        (printf "#lang typed/racket/base~n~n")
        (printf "(provide (all-defined-out))~n~n")
        (printf ";;; this file is auto-generated by digimon/digivice/linguist~n~n")

        (let print-comment ()
          (define line (read-line /dev/ymlin))

          (when (string? line)
            (cond [(regexp-match? #px"^[-]+$" line) (newline)]
                  [(string-prefix? line "#") (printf ";~a~n" (substring line 1)) (print-comment)]
                  [else (newline) (print-comment)])))

        (printf "(define-type Language-Misc-Datum (U String Symbol (Pairof Symbol (Listof Symbol)) (Pairof String (Listof String))))~n")
        (printf "(define-type Github-Language (List Natural String Symbol (Pairof Bytes (Listof Bytes)) (Option Keyword) (Immutable-HashTable Symbol Language-Misc-Datum)))~n~n")
        
        (define languages : Github-Languages
          (let language-fold ([langs : Github-Languages null])
            (define lang : (Option Github-Language) (linguist-read-language /dev/ymlin))

            (cond [(not lang) (reverse langs)]
                  [else (language-fold (cons lang langs))])))
        
        (pretty-write `(define git-languages : (Listof Github-Language) ',languages))))))

(define main : (-> (U (Listof String) (Vectorof String)) Void)
  (lambda [argument-list]
    (define-values (options λargv) (parse-linguist-flags argument-list))
    
    (cond [(linguist-flags-help? options) (display-linguist-flags)]
          [else (linguist-dump (λargv))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
