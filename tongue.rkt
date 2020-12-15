#lang typed/racket/base

(provide speak ~speak Tongue-Fold)
(provide current-tongue tongue-list default-tongue default-fallback-tongue)
(provide default-tongue-paths default-tongue-extension default-fold-tongue)
(provide (struct-out tongue-info))

(require racket/path)
(require racket/list)
(require racket/string)
(require racket/symbol)
(require racket/match)

(require "dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tongue-Fold (-> Path (Immutable-HashTable Symbol String) (Immutable-HashTable Symbol String)))
(struct tongue-info ([fold : Tongue-Fold] [extension : Bytes]) #:type-name Tongue-Info)

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
        (cond [(null? table) (lang-map 'en)]
              [(regexp-match (cadar table) system-lang) (lang-map (caar table))]
              [(regexp-match (caddar table) system-lang) (lang-map (caar table))]
              [else (check-next (cdr table))])))))

(define current-tongue : (Parameterof Symbol) (make-parameter (default-tongue)))
(define default-fallback-tongue : (Parameterof Symbol) (make-parameter 'en))
(define default-tongue-paths : (Parameterof (Listof Path-String)) (make-parameter null))

(define tongue-list : (-> (Listof Symbol))
  (lambda []
    (define all : (Listof Symbol)
      (for*/fold ([languages : (Listof Symbol) null])
                 ([tongue-root (in-list (default-tongue-paths))]
                  #:when (directory-exists? tongue-root)
                  [tongue.dir : Path (in-list (directory-list tongue-root #:build? #false))]
                  #:when (directory-exists? (build-path tongue-root tongue.dir)))
        (cons (string->symbol (path->string tongue.dir)) languages)))
    (sort (remove-duplicates all) symbol<?)))

(define speak : (-> Symbol [#:in Symbol] [#:dialect (Option Symbol)] [#:reload? Boolean] String)
  (lambda [word #:in [tongue (current-tongue)] #:dialect [dialect #false] #:reload? [reload? #false]]
    ; TODO: is it neccessary to downcase the `word`?
    (when (or reload? (not (hash-has-key? dictionary-base tongue)))
      (hash-set! dictionary-base tongue (load-tongues tongue)))
    (define text : (Option String)
      (cond [(symbol? dialect) (hash-ref (hash-ref (hash-ref dictionary-base tongue) dialect make-empty-dictionary) word sorry-for-not-found)]
            [else (for/or : (Option String) ([dialects (in-hash-values (hash-ref dictionary-base tongue))])
                    (hash-ref dialects word sorry-for-not-found))]))
    (cond [(string? text) text]
          [(eq? tongue (default-fallback-tongue)) (string-replace (symbol->immutable-string word) "-" " ")]
          [else (speak word #:in (default-fallback-tongue) #:dialect dialect #:reload? reload?)])))

(define ~speak : (-> Symbol [#:in Symbol] [#:dialect (Option Symbol)] [#:reload? Boolean] Any * String)
  (lambda [word #:in [tongue (current-tongue)] #:dialect [dialect #false] #:reload? [reload? #false] . argl]
    (cond [(null? argl) (speak word #:in tongue #:dialect dialect #:reload? reload?)]
          [else (apply format (speak word #:in tongue #:dialect dialect #:reload? reload?) argl)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define topic : Symbol 'exn:read:tongue)
(define dictionary-base : (HashTable Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol String))) (make-hasheq))

(define default-tongue-extension : Bytes #".rktl")
(define make-empty-dictionary : (-> (Immutable-HashTable Symbol String)) (λ [] (make-immutable-hasheq)))
(define sorry-for-not-found : (-> False) (λ [] #false))

(define default-fold-tongue : Tongue-Fold
  (lambda [tongue.rktl dictionary]
    (define records : Any (with-input-from-file tongue.rktl read))
    (cond [(list? records)
           (for/fold ([dict : (Immutable-HashTable Symbol String) dictionary])
                     ([record (in-list records)])
             (match record
               [(cons (? symbol? word) (? string? content)) (hash-set dict word content)]
               [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl record) dict]))]
          [else (dtrace-warning #:topic topic "~a: ~s" tongue.rktl records) dictionary])))

(define load-tongues : (-> Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol String)))
  (lambda [tongue]
    (define language : String (symbol->immutable-string tongue))
    (for/fold ([dictionaries : (Immutable-HashTable Symbol (Immutable-HashTable Symbol String)) (make-immutable-hasheq)])
              ([tongue-root (in-list (remove-duplicates (cons (collection-file-path "tongue" "digimon" "stone") (default-tongue-paths))))]
               #:when (directory-exists? (build-path tongue-root language)))
      (define-values (this-tongue-fold this-extension)
        (let ([reader.rkt (build-path tongue-root "reader.rkt")])
          (cond [(not (file-exists? reader.rkt)) (values default-fold-tongue default-tongue-extension)]
                [else (let ([& (dynamic-require reader.rkt 'tongue-info)])
                        (cond [(tongue-info? &) (values (tongue-info-fold &) (tongue-info-extension &))]
                              [else (values default-fold-tongue default-tongue-extension)]))])))
      (for/fold ([dictionaries : (Immutable-HashTable Symbol (Immutable-HashTable Symbol String)) dictionaries])
                ([dialect.ext (in-list (directory-list (build-path tongue-root language) #:build? #true))]
                 #:when (and (file-exists? dialect.ext)
                             (equal? (path-get-extension dialect.ext) this-extension)))
        (define dialect : Symbol (string->symbol (format "~a" (file-name-from-path (path-replace-extension dialect.ext #"")))))
        (with-handlers ([exn:fail? (λ [[e : exn]] (dtrace-warning #:topic topic "~a: ~a" dialect.ext (exn-message e)) dictionaries)])
          (hash-update dictionaries dialect
                       (λ [[dictionary : (Immutable-HashTable Symbol String)]]
                         (this-tongue-fold dialect.ext dictionary))
                       make-empty-dictionary))))))
