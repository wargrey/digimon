#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "linguist/languages.rkt")

(require "numstat.rkt")
(require "lstree.rkt")

(require "../system.rkt")
(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Langstat-Grouping-Option (U Boolean (Listof (Pairof String (U Bytes Github-Language-Extensions)))))

(struct (Git-Datum) git-language
  ([id : Index]
   [name : String]
   [type : Symbol]
   [color : (Option Keyword)]
   [extensions : Github-Language-Extensions]
   [content : Git-Datum])
  #:type-name Git-Language-With
  #:transparent)

(define git-default-subgroups : Git-Langstat-Grouping-Option '(["Scribble" . #".scrbl"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstats->langstats : (-> (Listof Git-Numstat) (Listof Symbol) Git-Langstat-Grouping-Option (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))))
  (let ([initial-stat : (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))) (hasheq)]
        [initial-substat : (Immutable-HashTable Index (Listof Git-Numstat-Line)) (hasheq)])
    (lambda [numstats types grouping-opt]
      (define languages : (Immutable-HashTable Index Github-Language) (github-load-language grouping-opt))
      
      (for/fold ([stats : (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))) initial-stat])
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
        (for/fold ([stats : (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))) stats])
                  ([(sub-id substat) (in-hash substats)])
          (define this-numstat : Git-Numstat ((inst cons Natural (Listof Git-Numstat-Line)) (car numstat) substat))
          (if (hash-has-key? stats sub-id)
              (let ([lang (hash-ref stats sub-id)])
                (hash-set stats sub-id (struct-copy git-language lang [content (cons this-numstat (git-language-content lang))])))
              (let-values ([(lang all-extensions) (github-language-group languages sub-id (eq? grouping-opt #true))])
                (hash-set stats (github-language-id lang)
                          (git-language (github-language-id lang) (github-language-name lang) (github-language-type lang)
                                        (github-language-color lang) all-extensions (list this-numstat))))))))))

(define git-numstats->additions+deletions : (-> (Listof Git-Numstat) (Listof (Pairof Natural (Pairof Natural Natural))))
  (lambda [numstats]
    (for/fold ([addition.deletions : (Listof (Pairof Natural (Pairof Natural Natural))) null])
              ([numstat (in-list (reverse numstats))])
      (define-values (adds dels)
        (for/fold ([adds : Natural 0] [dels : Natural 0])
                  ([line (in-list (cdr numstat))])
          (values (+ adds (vector-ref line 0))
                  (+ dels (vector-ref line 1)))))
      (cons (cons (car numstat)
                  (cons adds dels))
            addition.deletions))))

(define git-numstats->additions+deletions* : (-> (Listof Git-Numstat) (Values Natural Natural))
  (lambda [numstats]
    (define addition.deletions : (Listof (Pairof Natural (Pairof Natural Natural)))
      (git-numstats->additions+deletions numstats))

    (for/fold ([additions : Natural 0] [deletions : Natural 0])
              ([ts.add.del (in-list addition.deletions)])
      (values (+ additions (cadr ts.add.del))
              (+ deletions (cddr ts.add.del))))))

(define git-files->langfiles : (-> (Listof Git-File) (Listof Symbol) Git-Langstat-Grouping-Option (Immutable-HashTable Index (Git-Language-With (Listof Git-File))))
  (let ([initial-langfiles : (Immutable-HashTable Index (Git-Language-With (Listof Git-File))) (hasheq)])
    (lambda [files types grouping-opt]
      (define languages : (Immutable-HashTable Index Github-Language) (github-load-language grouping-opt))
      
      (for/fold ([langfiles : (Immutable-HashTable Index (Git-Language-With (Listof Git-File))) initial-langfiles])
                ([blob (in-list files)])
        (define ext (path-get-extension (git-file-pathname blob)))
        (cond [(not ext) langfiles]
              [(or (git-identify-language langfiles ext)
                   (github-identify-language languages ext types))
               => (λ [[id : Index]]
                    (if (hash-has-key? langfiles id)
                        (let ([lang (hash-ref langfiles id)])
                          (hash-set langfiles id (struct-copy git-language lang [content (cons blob (git-language-content lang))])))
                        (let-values ([(lang all-extensions) (github-language-group languages id (eq? grouping-opt #true))])
                          (hash-set langfiles (github-language-id lang)
                                    (git-language (github-language-id lang) (github-language-name lang) (github-language-type lang)
                                                  (github-language-color lang) all-extensions (list blob))))))]
              [else langfiles])))))

(define git-files->langsizes : (-> (Listof Git-File) (Listof Symbol) Git-Langstat-Grouping-Option (Immutable-HashTable Index (Git-Language-With Natural)))
  (let ([initial-langsizes : (Immutable-HashTable Index (Git-Language-With Natural)) (hasheq)])
    (lambda [files types grouping-opt]
      (define languages : (Immutable-HashTable Index Github-Language) (github-load-language grouping-opt))
      
      (for/fold ([langsizes : (Immutable-HashTable Index (Git-Language-With Natural)) initial-langsizes])
                ([blob (in-list files)])
        (define ext (path-get-extension (git-file-pathname blob)))
        (cond [(not ext) langsizes]
              [(or (git-identify-language langsizes ext)
                   (github-identify-language languages ext types))
               => (λ [[id : Index]]
                    (if (hash-has-key? langsizes id)
                        (let* ([lang (hash-ref langsizes id)]
                               [total (git-language-content lang)])
                          (hash-set langsizes id (struct-copy git-language lang [content (+ total (git-file-size blob))])))
                        (let-values ([(lang all-extensions) (github-language-group languages id (eq? grouping-opt #true))])
                          (hash-set langsizes (github-language-id lang)
                                    (git-language (github-language-id lang) (github-language-name lang) (github-language-type lang)
                                                  (github-language-color lang) all-extensions (git-file-size blob))))))]
              [else langsizes])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define github-load-language : (-> Git-Langstat-Grouping-Option (Immutable-HashTable Index Github-Language))
  (lambda [grouping-opt]
    (define languages0 : (Immutable-HashTable Index Github-Language)
      (read-language-metainfos* #:count-lines? #false
                                (parameterize ([current-digimon "digimon"])
                                  (digimon-path 'linguist "languages.yml"))))
    
    (cond [(pair? grouping-opt) (github-fork languages0 grouping-opt)]
          [else languages0])))

(define git-select-id : (-> (Listof Index) (Option Index))
  (lambda [ids]
    ; TODO: heuristically guessing the actual language is difficult for (re-)moved files
    (and (pair? ids)
         (apply min ids))))

(define git-identify-language : (All (Git-Datum) (case-> [(Immutable-HashTable Index (Git-Language-With Git-Datum)) Bytes -> (Option Index)]
                                                         [(Immutable-HashTable Index Any) Bytes (Immutable-HashTable Index Github-Language) -> (Option Index)]))
  (case-lambda
    [(stats ext)
     (let it ([pos : (Option Integer) (hash-iterate-first stats)]
              [ids : (Listof Index) null])
       (cond [(not pos) (git-select-id ids)]
             [else (let-values ([(lang-id metainfo) (hash-iterate-key+value stats pos)])
                     (if (member ext (git-language-extensions metainfo))
                         (it (hash-iterate-next stats pos) (cons lang-id ids))
                         (it (hash-iterate-next stats pos) ids)))]))]
    [(stats ext languages)
     (let it ([pos : (Option Integer) (hash-iterate-first stats)]
              [ids : (Listof Index) null])
       (cond [(not pos) (git-select-id ids)]
             [else (let ([lang-id (hash-iterate-key stats pos)])
                     (if (member ext (github-language-extensions (hash-ref languages lang-id)))
                         (it (hash-iterate-next stats pos) (cons lang-id ids))
                         (it (hash-iterate-next stats pos) ids)))]))]))

(define github-identify-language : (-> (Immutable-HashTable Index Github-Language) Bytes (Listof Symbol) (Option Index))
  (lambda [languages ext types]
    (let it ([pos : (Option Integer) (hash-iterate-first languages)]
             [ids : (Listof Index) null])
      (cond [(not pos) (git-select-id ids)]
            [else (let-values ([(lang-id metainfo) (hash-iterate-key+value languages pos)])
                    (cond [(not (member ext (github-language-extensions metainfo))) (it (hash-iterate-next languages pos) ids)]
                          [(and (pair? types) (not (memq (github-language-type metainfo) types))) (it (hash-iterate-next languages pos) ids)]
                          [else (it (hash-iterate-next languages pos) (cons lang-id ids))]))]))))

(define github-language-group : (-> (Immutable-HashTable Index Github-Language) Natural Boolean (Values Github-Language Github-Language-Extensions))
  (lambda [languages id grouping?]
    (define self-lang : Github-Language (hash-ref languages id))
    
    (if (and grouping?)
        (let* ([maybe-parent (github-language-parent self-lang)]
               [parent-lang (and maybe-parent (github-lookup-parent languages maybe-parent))])
          (cond [(not parent-lang) (values self-lang (github-language-extensions self-lang))]
                [else (values parent-lang (append (github-language-extensions self-lang) (github-language-extensions parent-lang)))]))
        (values self-lang (github-language-extensions self-lang)))))

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
