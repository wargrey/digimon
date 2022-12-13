#lang typed/racket/base

(provide (all-defined-out) Git-Numstat Git-Numstat-Line)

(require racket/path)
(require racket/port)

(require "digitama/git.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-root : (->* () (Path-String) (Option Path-String))
  (lambda [[dir (current-directory)]]
    (define .git (build-path dir ".git"))
    (cond [(directory-exists? .git) .git]
          [else (let-values ([(base name dir?) (split-path (simple-form-path dir))])
                  (and (path? base) (git-root base)))])))

(define git-numstat : (-> [#:group-by-day? Boolean] (Listof Git-Numstat))
  (lambda [#:group-by-day? [day? #true]]
    (define git (find-executable-path "git"))
    (cond [(or (not (git-root)) (not git)) null]
          [else (append (git-numstat-exec day? git "diff" "--numstat")
                        (git-numstat-exec day? git "log" git:%at "--numstat" "--no-merges"))])))
