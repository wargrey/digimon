#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)

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

(define wisemon-phony-goal-ref : (-> Symbol (Option Wisemon-Phony))
  (lambda [goal]
    (define strgoal : String (symbol->immutable-string goal))
    (define goal.rkt : Path (build-path (wisemon-phony-goal-rootdir) (string-append strgoal ".rkt")))

    (or (and (file-exists? goal.rkt)
             (let ([heuristic-sym (string->symbol (string-append strgoal "-phony-goal"))])
               (define maybe-goal (dynamic-require goal.rkt heuristic-sym))
               (cond [(and (wisemon-phony? maybe-goal) (eq? (wisemon-phony-name maybe-goal) goal)) maybe-goal]
                     [else #false])))
        (hash-ref (wisemon-list-phony-goals) goal (λ [] #false)))))

(define wisemon-list-phony-goals : (-> (Immutable-HashTable Symbol Wisemon-Phony))
  (lambda []
    (for/fold ([Phonies : (Immutable-HashTable Symbol Wisemon-Phony) (make-immutable-hasheq)])
              ([phony.rkt (in-directory (wisemon-phony-goal-rootdir))] #:when (regexp-match? #px".rkt$" phony.rkt))
      (dynamic-require phony.rkt #false)
      (parameterize ([current-namespace (module->namespace phony.rkt)])
        (for/fold ([phonies : (Immutable-HashTable Symbol Wisemon-Phony) Phonies])
                  ([sym (in-list (namespace-mapped-symbols))])
          (define maybe-goal (namespace-variable-value sym #false (λ _ #false)))
          (cond [(not (wisemon-phony? maybe-goal)) phonies]
                [else (hash-set phonies (wisemon-phony-name maybe-goal) maybe-goal)]))))))
