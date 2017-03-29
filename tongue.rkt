#lang typed/racket

(provide speak ~speak)
(provide current-tongue all-tongues default-tongue default-fallback-tongue)
(provide default-tongue-paths default-tongue-extension default-tongue-fold)

(require "system.rkt")

(define default-tongue : (->* () ((-> Symbol Symbol)) Symbol)
  (lambda [[lang-map identity]]
    (define-type Tongue-Table (Listof (List Symbol Regexp Regexp)))
    (define default-table : Tongue-Table
      '((English             #rx"^en_"        #rx"^English_")
        (Spanish             #rx"^es_"        #rx"^Espanol_")
        (German              #rx"^de_"        #rx"^German_")
        (French              #rx"^fr_"        #rx"French_")
        (Dutch               #rx"nl_"         #rx"^Netherlands_")
        (Danish              #rx"^da_DK"      #rx"^Danish_")
        (Portuguese          #rx"^pt_"        #rx"Portuguese_")
        (Japanese            #rx"^ja_"        #rx"^Japan_")
        (Traditional-Chinese #rx"^zh_(HK|TW)" #rx"Chinese_(Hong|Taiwan)")
        (Simplified-Chinese  #rx"^zh_CN"      #rx"Chinese_China")
        (Russian             #rx"^ru_"        #rx"^Russian_")
        (Ukrainian           #rx"^uk_"        #rx"^Ukrainian_")
        (Korean              #rx"^ko_"        #rx"^Korean_")))

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
                   (fold-tongue tongue.rktl dictionary))))
    (hash-ref (hash-ref dicts tongue) word
              (thunk (cond [(symbol=? tongue (default-fallback-tongue)) (string-replace (symbol->string word) "-" " ")]
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
    (define records : Any (with-handlers ([exn? (λ [[e : exn]] e)]) (with-input-from-file tongue.rktl read)))
    (cond [(list? records)
           (for/fold ([dict : (HashTable Symbol String) dictionary])
                     ([record (in-list records)])
             (match record
               [(cons (? symbol? word) (? string? content)) (hash-set dict word content)]
               [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl record) dict]))]
          [(exn? records) (dtrace-warning #:topic topic "~a: ~a" tongue.rktl (exn-message records)) dictionary]
          [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl records) dictionary])))
