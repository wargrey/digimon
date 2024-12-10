#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Lang-Shell (-> (Listof String) Thread Any))

(struct nanomon-shell
  ([name : Symbol]
   [exec : Lang-Shell]
   [description : String])
  #:constructor-name make-nanomon-shell
  #:type-name Nanomon-Shell
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-make-shell : (-> #:name Symbol #:shell Lang-Shell #:desc String Nanomon-Shell)
  (lambda [#:name name #:shell shell #:desc desc]
    (make-nanomon-shell name shell desc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-shell-rootdir : (-> Path)
  (lambda []
    (collection-file-path "shell" "digimon" "digivice" "nanomon")))
  
(define nanomon-shell-ref : (->* (Symbol) (Path) (Option Nanomon-Shell))
  (lambda [shell [rootdir (nanomon-shell-rootdir)]]
    (define strshell : String (symbol->immutable-string shell))
    (define shell.rkt : Path (build-path (nanomon-shell-rootdir) (string-append strshell ".rkt")))

    (or (and (file-exists? shell.rkt)
             (let ([heuristic-sym (string->symbol (string-append strshell "-shell"))])
               (define maybe-shell (dynamic-require shell.rkt heuristic-sym))
               (cond [(and (nanomon-shell? maybe-shell) (eq? (nanomon-shell-name maybe-shell) shell)) maybe-shell]
                     [else #false])))
        (hash-ref (nanomon-list-shells rootdir) shell
                  (λ [] (hash-ref (nanomon-list-foreign-shells) shell
                                  (λ [] #false)))))))

(define nanomon-list-shells-from-file : (-> Path (Immutable-HashTable Symbol Nanomon-Shell) (Immutable-HashTable Symbol Nanomon-Shell))
  (lambda [shell.rkt shells0]
    (dynamic-require shell.rkt #false)
    (parameterize ([current-namespace (module->namespace shell.rkt)])
      (for/fold ([shells : (Immutable-HashTable Symbol Nanomon-Shell) shells0])
                ([sym (in-list (namespace-mapped-symbols))])
        (define maybe-shell (namespace-variable-value sym #false (λ _ #false)))
        (cond [(not (nanomon-shell? maybe-shell)) shells]
              [else (hash-set shells (nanomon-shell-name maybe-shell) maybe-shell)])))))

(define nanomon-list-shells : (->* () (Path (Immutable-HashTable Symbol Nanomon-Shell)) (Immutable-HashTable Symbol Nanomon-Shell))
  (lambda [[rootdir (nanomon-shell-rootdir)] [shells0 ((inst make-immutable-hasheq Symbol Nanomon-Shell))]]
    (for/fold ([shells : (Immutable-HashTable Symbol Nanomon-Shell) shells0])
              ([shell.rkt (in-directory rootdir)] #:when (regexp-match? #px".rkt$" shell.rkt))
      (nanomon-list-shells-from-file shell.rkt shells))))

(define nanomon-list-foreign-shells : (-> (Immutable-HashTable Symbol Nanomon-Shell))
  (lambda []
    (for/fold ([shells : (Immutable-HashTable Symbol Nanomon-Shell) (make-immutable-hasheq)])
              ([path (in-list (apply append (map nanomon-foreign-shell-path-filter (find-relevant-directories (list nanomon-foreign-shell-id)))))])
      (cond [(file-exists? path) (nanomon-list-shells-from-file path shells)]
            [(directory-exists? path) (nanomon-list-shells path shells)]
            [else shells]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-foreign-shell-id : Symbol 'nanomon-shell)

(define nanomon-foreign-shell-path-filter : (-> Path (Listof Path))
  (lambda [infodir]
    (define info-ref (get-info/full infodir))

    (cond [(not info-ref) null]
          [else (let ([shell-info (info-ref nanomon-foreign-shell-id (λ [] #false))])
                  (cond [(path-string? shell-info) (list (build-path infodir shell-info))]
                        [(list? shell-info) (filter-map (λ [[v : Any]] (and (path-string? v) (build-path infodir v))) shell-info)]
                        [else null]))])))
