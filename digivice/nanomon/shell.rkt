#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Lang-Shell (-> Path Thread Any))

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
  
(define nanomon-shell-ref : (-> Symbol (Option Nanomon-Shell))
  (lambda [shell]
    (define strshell : String (symbol->immutable-string shell))
    (define shell.rkt : Path (build-path (nanomon-shell-rootdir) (string-append strshell ".rkt")))

    (or (and (file-exists? shell.rkt)
             (let ([heuristic-sym (string->symbol (string-append strshell "-shell"))])
               (define maybe-shell (dynamic-require shell.rkt heuristic-sym))
               (cond [(and (nanomon-shell? maybe-shell) (eq? (nanomon-shell-name maybe-shell) shell)) maybe-shell]
                     [else #false])))
        (hash-ref (nanomon-list-shells) shell (λ [] #false)))))

(define nanomon-list-shells : (-> (Immutable-HashTable Symbol Nanomon-Shell))
  (lambda []
    (for/fold ([shells : (Immutable-HashTable Symbol Nanomon-Shell) (make-immutable-hasheq)])
              ([shell.rkt (in-directory (nanomon-shell-rootdir))] #:when (regexp-match? #px".rkt$" shell.rkt))
      (dynamic-require shell.rkt #false)
      (parameterize ([current-namespace (module->namespace shell.rkt)])
        (for/fold ([shells : (Immutable-HashTable Symbol Nanomon-Shell) shells])
                  ([sym (in-list (namespace-mapped-symbols))])
          (define maybe-shell (namespace-variable-value sym #false (λ _ #false)))
          (cond [(not (nanomon-shell? maybe-shell)) shells]
                [else (hash-set shells (nanomon-shell-name maybe-shell) maybe-shell)]))))))
