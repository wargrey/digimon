#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "linguist.rkt")
(require "numstat.rkt")

(require "../system.rkt")
(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Langstat-Grouping-Option (U Boolean (Listof (Pairof String (U Bytes Github-Language-Extensions)))))

(struct git-language
  ([id : Index]
   [name : String]
   [type : Symbol]
   [color : (Option Keyword)]
   [extensions : Github-Language-Extensions]
   [lines : (Listof Git-Numstat)])
  #:type-name Git-Language
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstats->langstat : (-> (Listof Git-Numstat) (Listof Symbol) Git-Langstat-Grouping-Option (Immutable-HashTable Index Git-Language))
  (let ([initial-stat : (Immutable-HashTable Index Git-Language) (hasheq)]
        [initial-substat : (Immutable-HashTable Index (Listof Git-Numstat-Line)) (hasheq)])
    (lambda [numstats types grouping-opt]
      (define languages0 : (Immutable-HashTable Index Github-Language)
        (read-language-metainfos* #:count-lines? #false
                                  (parameterize ([current-digimon "digimon"])
                                    (digimon-path 'language "github.yml"))))

      (define languages : (Immutable-HashTable Index Github-Language)
        (cond [(pair? grouping-opt) (time (github-fork languages0 grouping-opt))]
              [else languages0]))
      
      (for/fold ([stats : (Immutable-HashTable Index Git-Language) initial-stat])
                ([numstat (in-list numstats)])
        (define substats : (Immutable-HashTable Index (Listof Git-Numstat-Line))
          (for/fold ([substats : (Immutable-HashTable Index (Listof Git-Numstat-Line)) initial-substat])
                    ([num-line (in-list (cdr numstat))])
            (define filename (let ([fn (vector-ref num-line 2)]) (if (pair? fn) (cdr fn) fn)))
            (define ext (path-get-extension filename))
            (cond [(not ext) substats]
                  [(git-identify-language stats ext)
                   => (λ [[id : Index]] (hash-set substats id (cons num-line (hash-ref substats id (inst list Git-Numstat-Line)))))]
                  [(git-identify-language substats ext languages)
                   => (λ [[id : Index]] (hash-set substats id (cons num-line (hash-ref substats id (inst list Git-Numstat-Line)))))]
                  [(github-identify-language languages ext types)
                   => (λ [[id : Index]] (hash-set substats id (cons num-line (hash-ref substats id (inst list Git-Numstat-Line)))))]
                  [else substats])))
        (for/fold ([stats : (Immutable-HashTable Index Git-Language) stats])
                  ([(sub-id substat) (in-hash substats)])
          (define this-numstat : Git-Numstat ((inst cons Natural (Listof Git-Numstat-Line)) (car numstat) substat))
          (if (hash-has-key? stats sub-id)
              (let ([lang (hash-ref stats sub-id)])
                (hash-set stats sub-id (struct-copy git-language lang [lines (cons this-numstat (git-language-lines lang))])))
              (let ([self-lang (hash-ref languages sub-id)])
                (define-values (lang all-extensions)
                  (cond [(eq? grouping-opt #true)
                         (let* ([maybe-parent (github-language-parent (hash-ref languages sub-id))]
                                [parent-lang (and maybe-parent (github-lookup-parent languages maybe-parent))])
                           (cond [(not parent-lang) (values self-lang (github-language-extensions self-lang))]
                                 [else (values parent-lang (append (github-language-extensions self-lang) (github-language-extensions parent-lang)))]))]
                        [else (values self-lang (github-language-extensions self-lang))]))
                (hash-set stats (github-language-id lang)
                          (git-language (github-language-id lang) (github-language-name lang) (github-language-type lang)
                                        (github-language-color lang) all-extensions (list this-numstat))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-identify-language : (case-> [(Immutable-HashTable Index Git-Language) Bytes -> (Option Index)]
                                        [(Immutable-HashTable Index Any) Bytes (Immutable-HashTable Index Github-Language) -> (Option Index)])
  (case-lambda
    [(stats ext)
     (let it ([pos : (Option Integer) (hash-iterate-first stats)])
       (cond [(not pos) #false]
             [else (let-values ([(lang-id metainfo) (hash-iterate-key+value stats pos)])
                     (cond [(member ext (git-language-extensions metainfo)) lang-id]
                           [else (it (hash-iterate-next stats pos))]))]))]
    [(stats ext languages)
     (let it ([pos : (Option Integer) (hash-iterate-first stats)])
       (cond [(not pos) #false]
             [else (let ([lang-id (hash-iterate-key stats pos)])
                     (cond [(member ext (github-language-extensions (hash-ref languages lang-id))) lang-id]
                           [else (it (hash-iterate-next stats pos))]))]))]))

(define github-identify-language : (-> (Immutable-HashTable Index Github-Language) Bytes (Listof Symbol) (Option Index))
  (lambda [languages ext types]
    (let it ([pos : (Option Integer) (hash-iterate-first languages)]
             [ids : (Listof Index) null])
      (if (not pos)

          ; TODO: heuristically guessing the actual language is difficult for (re-)moved files
          (and (pair? ids)
               (apply min ids))

          (let-values ([(lang-id metainfo) (hash-iterate-key+value languages pos)])
            (cond [(not (member ext (github-language-extensions metainfo))) (it (hash-iterate-next languages pos) ids)]
                  [(and (pair? types) (not (memq (github-language-type metainfo) types))) (it (hash-iterate-next languages pos) ids)]
                  [else (it (hash-iterate-next languages pos) (cons lang-id ids))]))))))

(define github-lookup-parent : (-> (Immutable-HashTable Index Github-Language) String (Option Github-Language))
  (lambda [languages parent]
    (for/or : (Option Github-Language) ([lang (in-hash-values languages)])
      (and (string-ci=? (github-language-name lang) parent)
           lang))))

(define github-fork : (-> (Immutable-HashTable Index Github-Language) (Listof (Pairof String (U Bytes Github-Language-Extensions)))
                          (Immutable-HashTable Index Github-Language))
  (let ([initial-lang : (Immutable-HashTable Index Github-Language) (hasheq)])
    (lambda [languages0 children0]
      (define children : (Listof (Pairof String Github-Language-Extensions))
        (for/list ([child (in-list children0)])
          ((inst cons String Github-Language-Extensions)
           (car child) (let ([es (cdr child)])
                         (if (bytes? es) (list es) es)))))
      
      (let try-fork-next ([languages : (Immutable-HashTable Index Github-Language) initial-lang]
                          [pos : (Option Integer) (hash-iterate-first languages0)]
                          [children : (Listof (Pairof String Github-Language-Extensions)) children])
        (cond [(not pos) languages]
              [else (let*-values ([(id lang) (hash-iterate-key+value languages0 pos)]
                                  [(forks rest) (github-lookup-forks lang children)])
                      (cond [(null? forks) (try-fork-next (hash-set languages id lang) (hash-iterate-next languages0 pos) rest)]
                            [else (try-fork-next (for/fold ([lang++ : (Immutable-HashTable Index Github-Language) languages])
                                                           ([fork (in-list forks)])
                                                   (hash-set lang++ (github-language-id fork) fork))
                                                 (hash-iterate-next languages0 pos)
                                                 rest)]))])))))

(define github-lookup-forks : (-> Github-Language (Listof (Pairof String Github-Language-Extensions))
                                  (Values (Listof Github-Language) (Listof (Pairof String Github-Language-Extensions))))
  (lambda [lang children]
    (let try-fork ([infos : (Listof (Pairof String Github-Language-Extensions)) children]
                   [pexts : (Listof Bytes) (github-language-extensions lang)]
                   [forks : (Listof Github-Language) null]
                   [sofni : (Listof (Pairof String Github-Language-Extensions)) null])
      (cond [(pair? infos)
             (let-values ([(self rest) (values (car infos) (cdr infos))])
               (define sexts : (Listof Bytes) (filter (λ [[e : Bytes]] (member e pexts)) (cdr self)))
               (cond [(null? sexts) (try-fork rest pexts forks (cons self sofni))]
                     [else (let ([fork (struct-copy github-language lang [id (github-fork-id lang (length forks))] [name (car self)] [extensions sexts])])
                             (try-fork rest (remove* sexts pexts) (cons fork forks) sofni))]))]
            [(pair? forks)
             (cond [(null? pexts) (values forks sofni)]
                   [else (values (cons (struct-copy github-language lang [extensions pexts]) forks) sofni)])]
            [else (values null children)]))))

(define github-fork-id : (-> Github-Language Index Index)
  (lambda [lang idx]
    ; Suppose github won't use numbers that greater than
    ;   #xFFFFFFFF (32bit maximum natural, 4294967295 in decimal)
    ;   as language ids.
    ; So, we choose 5000000000 as the base number for forked languages
    ;   so that we could easily figure out the forking ones before
    ;   printing them as hexadecimals
    (unsafe-idx+ (unsafe-idx* (+ idx 5) 1000000000)
                 (github-language-id lang))))
