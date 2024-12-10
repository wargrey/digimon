#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Make-Info-Phony (-> String Info-Ref Any))
(define-type Make-Free-Phony (-> String (Option Info-Ref) Any))

(struct wisemon-phony ([name : Symbol] [description : String]) #:type-name Wisemon-Phony #:transparent)
(struct wisemon-info-phony wisemon-phony ([make : Make-Info-Phony]) #:type-name Wisemon-Info-Phony #:transparent)
(struct wisemon-free-phony wisemon-phony ([make : Make-Free-Phony]) #:type-name Wisemon-Free-Phony #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make-info-phony : (-> #:name Symbol #:phony Make-Info-Phony #:desc String Wisemon-Phony)
  (lambda [#:name name #:phony phony #:desc desc]
    (wisemon-info-phony name desc phony)))

(define wisemon-make-free-phony : (-> #:name Symbol #:phony Make-Free-Phony #:desc String Wisemon-Phony)
  (lambda [#:name name #:phony phony #:desc desc]
    (wisemon-free-phony name desc phony)))

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
    (for/fold ([phonies : (Immutable-HashTable Symbol Wisemon-Phony) phonies0])
              ([phony.rkt (in-directory rootdir)] #:when (regexp-match? #px".rkt$" phony.rkt))
      (wisemon-list-phony-goals-from-file phony.rkt phonies))))

(define wisemon-list-foreign-phony-goals : (-> (Immutable-HashTable Symbol Wisemon-Phony))
  (lambda []
    (for/fold ([phonies : (Immutable-HashTable Symbol Wisemon-Phony) (make-immutable-hasheq)])
              ([path (in-list (apply append (map wisemon-foreign-phony-path-filter (find-relevant-directories (list wisemon-foreign-phony-id)))))])
      (cond [(file-exists? path) (wisemon-list-phony-goals-from-file path phonies)]
            [(directory-exists? path) (wisemon-list-phony-goals path phonies)]
            [else phonies]))))

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
