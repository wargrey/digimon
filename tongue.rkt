#lang typed/racket/base

(provide speak ~speak)
(provide current-tongue all-tongues default-tongue default-fallback-tongue)
(provide default-tongue-paths default-tongue-extension default-tongue-fold)

(require racket/path)
(require racket/list)
(require racket/string)
(require racket/format)
(require racket/match)

(require "system.rkt")

; https://www.w3.org/International/questions/qa-lang-2or3.en.html
; https://www.w3.org/International/articles/language-tags
; http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry

(define default-tongue : (->* () ((-> Symbol Symbol)) Symbol)
  (lambda [[lang-map values]]
    (define-type Tongue-Table (Listof (List Symbol Regexp Regexp)))
    (define default-table : Tongue-Table
      '((en      #rx"^en_"        #rx"^English_")
        (es      #rx"^es_"        #rx"^Espanol_")
        (de      #rx"^de_"        #rx"^German_")
        (fr      #rx"^fr_"        #rx"French_")
        (nl      #rx"nl_"         #rx"^Netherlands_")
        (da      #rx"^da_DK"      #rx"^Danish_")
        (pt      #rx"^pt_"        #rx"Portuguese_")
        (ja      #rx"^ja_"        #rx"^Japan_")
        (zh-Hant #rx"^zh_(HK|TW)" #rx"Chinese_(Hong|Taiwan)")
        (zh-Hans #rx"^zh_CN"      #rx"Chinese_China")
        (ru      #rx"^ru_"        #rx"^Russian_")
        (uk      #rx"^uk_"        #rx"^Ukrainian_")
        (ko      #rx"^ko_"        #rx"^Korean_")))

    (let ([system-lang (system-language+country)])
      (let check-next : Symbol ([table : Tongue-Table default-table])
        (cond [(null? table) (lang-map 'English)]
              [(regexp-match (cadar table) system-lang) (lang-map (caar table))]
              [(regexp-match (caddar table) system-lang) (lang-map (caar table))]
              [else (check-next (cdr table))])))))

(define current-tongue : (Parameterof Symbol) (make-parameter (default-tongue)))
(define default-fallback-tongue : (Parameterof Symbol) (make-parameter 'English))
(define default-tongue-paths : (Parameterof (Listof Path-String)) (make-parameter null))
(define default-tongue-extension : (Parameterof Bytes) (make-parameter #".rktl"))
(define default-tongue-fold : (Parameterof (Option (-> Path (HashTable Symbol String) (HashTable Symbol String)))) (make-parameter #false))

(define all-tongues : (-> (Listof Symbol))
  (lambda []
    (define all : (Listof Symbol)
      (for*/fold ([languages : (Listof Symbol) null])
                 ([digimon-tongue (in-list (default-tongue-paths))]
                  #:when (directory-exists? digimon-tongue)
                  [tongue.rktl : Path (in-list (directory-list digimon-tongue))]
                  #:when (equal? (path-get-extension tongue.rktl) (default-tongue-extension)))
        (cons (string->symbol (path->string (path-replace-extension tongue.rktl #""))) languages)))
    (sort (remove-duplicates all) symbol<?)))

(define speak : (-> Symbol [#:in Symbol] String)
  (lambda [word #:in [tongue (current-tongue)]]
    (unless (hash-has-key? dicts tongue)
      (define fold-tongue : (-> Path (HashTable Symbol String) (HashTable Symbol String)) (or (default-tongue-fold) default-fold-tongue))
      (hash-set! dicts tongue
                 (for/fold ([dictionary : (HashTable Symbol String) (make-immutable-hasheq)])
                           ([digimon-tongue (in-list (default-tongue-paths))])
                   (define tongue.rktl : Path (build-path digimon-tongue (~a tongue (default-tongue-extension))))
                   (with-handlers ([exn:fail? (λ [[e : exn]] (dtrace-warning #:topic topic "~a: ~a" tongue.rktl (exn-message e)) dictionary)])
                     (fold-tongue tongue.rktl dictionary)))))
    (hash-ref (hash-ref dicts tongue) word ; TODO: is it neccessary to downcase the word?
              (λ [] (cond [(eq? tongue (default-fallback-tongue)) (string-replace (symbol->string word) "-" " ")]
                          [else (speak word #:in (default-fallback-tongue))])))))

(define ~speak : (-> Symbol [#:in Symbol] Any * String)
  (lambda [word #:in [tongue (current-tongue)] . argl]
    (cond [(null? argl) (speak word #:in tongue)]
          [else (apply format (speak word #:in tongue) argl)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define topic : Symbol 'exn:read:tongue)
(define dicts : (HashTable Symbol (HashTable Symbol String)) (make-hasheq))

(define default-fold-tongue : (-> Path (HashTable Symbol String) (HashTable Symbol String))
  (lambda [tongue.rktl dictionary]
    (define dictionary : (HashTable Symbol String) (make-immutable-hasheq))
    (define records : Any (with-input-from-file tongue.rktl read))
    (cond [(list? records)
           (for/fold ([dict : (HashTable Symbol String) dictionary])
                     ([record (in-list records)])
             (match record
               [(cons (? symbol? word) (? string? content)) (hash-set dict word content)]
               [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl record) dict]))]
          [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl records) dictionary])))
