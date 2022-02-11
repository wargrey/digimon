#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Make-Phony (-> String Info-Ref Any))

(struct wisemon-phony
  ([name : Symbol]
   [make : Make-Phony]
   [description : String])
  #:constructor-name make-wisemon-phony
  #:type-name Wisemon-Phony
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make-phony : (-> #:name Symbol #:phony Make-Phony #:desc String Wisemon-Phony)
  (lambda [#:name name #:phony phony #:desc desc]
    (make-wisemon-phony name phony desc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-phony-goal-rootdir : (-> Path)
  (lambda []
    (collection-file-path "phony" "digimon" "digivice" "wisemon")))

(define wisemon-phony-goal-ref : (->* (Symbol) (Path) (Option Wisemon-Phony))
  (lambda [goal [rootdir (wisemon-phony-goal-rootdir)]]
    (define strgoal : String (symbol->immutable-string goal))
    (define goal.rkt : Path (build-path rootdir (string-append strgoal ".rkt")))

    (or (and (file-exists? goal.rkt)
             (let ([heuristic-sym (string->symbol (string-append strgoal "-phony-goal"))])
               (define maybe-goal (dynamic-require goal.rkt heuristic-sym))
               (cond [(and (wisemon-phony? maybe-goal) (eq? (wisemon-phony-name maybe-goal) goal)) maybe-goal]
                     [else #false])))
        (hash-ref (wisemon-list-phony-goals rootdir) goal
                  (λ [] (hash-ref (wisemon-list-foreign-phony-goals) goal
                                  (λ [] #false)))))))

(define wisemon-list-phony-goals-from-file : (-> Path (Immutable-HashTable Symbol Wisemon-Phony) (Immutable-HashTable Symbol Wisemon-Phony))
  (lambda [phony.rkt phonies0]
    (dynamic-require phony.rkt #false)
    (parameterize ([current-namespace (module->namespace phony.rkt)])
      (for/fold ([phonies : (Immutable-HashTable Symbol Wisemon-Phony) phonies0])
                ([sym (in-list (namespace-mapped-symbols))])
        (define maybe-goal (namespace-variable-value sym #false (λ _ #false)))
        (cond [(not (wisemon-phony? maybe-goal)) phonies]
              [else (hash-set phonies (wisemon-phony-name maybe-goal) maybe-goal)])))))

(define wisemon-list-phony-goals : (->* () (Path (Immutable-HashTable Symbol Wisemon-Phony)) (Immutable-HashTable Symbol Wisemon-Phony))
  (lambda [[rootdir (wisemon-phony-goal-rootdir)] [phonies0 ((inst make-immutable-hasheq Symbol Wisemon-Phony))]]
    (for/fold ([Phonies : (Immutable-HashTable Symbol Wisemon-Phony) phonies0])
              ([phony.rkt (in-directory rootdir)] #:when (regexp-match? #px".rkt$" phony.rkt))
      (wisemon-list-phony-goals-from-file phony.rkt Phonies))))

(define wisemon-list-foreign-phony-goals : (-> (Immutable-HashTable Symbol Wisemon-Phony))
  (lambda []
    (for/fold ([Phonies : (Immutable-HashTable Symbol Wisemon-Phony) (make-immutable-hasheq)])
              ([path (in-list (apply append (map wisemon-foreign-phony-path-filter (find-relevant-directories (list wisemon-foreign-phony-id)))))])
      (cond [(file-exists? path) (wisemon-list-phony-goals-from-file path Phonies)]
            [(directory-exists? path) (wisemon-list-phony-goals path Phonies)]
            [else Phonies]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-foreign-phony-id : Symbol 'wisemon-phony)

(define wisemon-foreign-phony-path-filter : (-> Path (Listof Path))
  (lambda [infodir]
    (define info-ref (get-info/full infodir))

    (cond [(not info-ref) null]
          [else (let ([phony-info (info-ref wisemon-foreign-phony-id (λ [] #false))])
                  (cond [(path-string? phony-info) (list (build-path infodir phony-info))]
                        [(list? phony-info) (filter-map (λ [[v : Any]] (and (path-string? v) (build-path infodir v))) phony-info)]
                        [else null]))])))
