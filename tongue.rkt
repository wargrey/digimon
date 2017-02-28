#lang typed/racket

(provide speak ~speak digimon-tongue-paths)
(provide current-tongue all-tongues default-tongue default-fallback-tongue)

(require "system.rkt")

;;; Although Racket has its own i18n solution, it is binded too close with DrRacket.
;;; So I write my own solution based on its idea, but more elegant.

;; The first regexp is used in Windows and the second is used on other platforms.
;; All regexps are compared to the result of (system-language+country).
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

(define default-tongue : (-> Symbol)
  (lambda []
    (let ([system-lang (system-language+country)])
      (let check-next : Symbol ([table : Tongue-Table default-table])
        (match-define (list (list lang win unix) table/rest ...) table)
        (cond [(regexp-match win system-lang) lang]
              [(regexp-match unix system-lang) lang]
              [(null? table/rest) 'English]
              [else (check-next table/rest)])))))

(define current-tongue : (Parameterof Symbol) (make-parameter (default-tongue)))
(define default-fallback-tongue : (Parameterof Symbol) (make-parameter 'English))
(define digimon-tongue-paths : (Parameterof (Listof Path-String)) (make-parameter null))

(define all-tongues : (-> (Listof Symbol))
  (lambda []
    (sort (remove-duplicates
           (for*/fold ([languages : (Listof Symbol) null])
                      ([digimon-tongue (in-list (digimon-tongue-paths))]
                       #:when (directory-exists? digimon-tongue)
                       [tongue.rktl : Path (in-list (directory-list digimon-tongue))]
                       #:when (equal? (filename-extension tongue.rktl) #"rktl"))
             (cons (string->symbol (string-trim (path->string tongue.rktl) #px"\\.rktl")) languages)))
          symbol<?)))

(define speak : (-> Symbol [#:in Symbol] String)
  (let ([dicts : (HashTable Symbol (HashTable Symbol String)) (make-hasheq)]
        [topic : Symbol 'exn:read:tongue])
    (lambda [word #:in [tongue (current-tongue)]]
      (unless (hash-has-key? dicts tongue)
        (hash-set! dicts tongue
                   (for/fold ([dictionary : (HashTable Symbol String) (make-immutable-hasheq)])
                             ([digimon-tongue (in-list (digimon-tongue-paths))])
                     (define tongue.rktl : Path (build-path digimon-tongue (~a tongue #".rktl")))
                     (define records : Any (with-handlers ([exn? (λ [[e : exn]] e)]) (with-input-from-file tongue.rktl read)))
                     (cond [(list? records)
                            (for/fold ([dict : (HashTable Symbol String) dictionary])
                                      ([record (in-list records)])
                              (match record
                                [(cons (? symbol? word) (? string? content)) (hash-set dict word content)]
                                [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl record) dict]))]
                           [(exn? records) (dtrace-warning #:topic topic "~a: ~a" tongue.rktl (exn-message records)) dictionary]
                           [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl records) dictionary]))))
      (hash-ref (hash-ref dicts tongue) word
                (thunk (cond [(symbol=? tongue (default-fallback-tongue)) (string-replace (symbol->string word) "-" " ")]
                             [else (speak word #:in (default-fallback-tongue))]))))))

(define ~speak : (-> Symbol [#:in Symbol] Any * String)
  (lambda [word #:in [tongue (current-tongue)] . argl]
    (cond [(null? argl) (speak word #:in tongue)]
          [else (apply format (speak word #:in tongue) argl)])))
