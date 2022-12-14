#lang typed/racket/base

(provide (all-defined-out) Git-Numstat Git-Numstat-Line)

(require racket/path)
(require racket/port)

(require "digitama/git.rkt")
(require "digitama/exec.rkt")

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
          [else (let ([numstat-fold (git-numstat-make-fold (and day? s/day))])
                  (reverse (fg-recon-exec* 'git-numstat git (list (list "log") (list git:%at "--numstat" "--no-merges")) numstat-fold
                                           (fg-recon-exec* 'git-numstat git (list (list "diff") (list "--numstat")) numstat-fold null))))])))
