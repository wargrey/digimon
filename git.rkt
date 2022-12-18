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

(define git-numstat : (-> [#:group-by-day? Boolean] [#:no-renames? Boolean] [#:since (Option Git-Date-Datum)] [#:until (Option Git-Date-Datum)] [#:localtime? Boolean] 
                          [#:n (Option Natural)] [#:authors (U String (Listof String))] [#:committers (U String (Listof String))] [#:with-diff? Boolean]
                          (Listof Git-Numstat))
  (lambda [#:no-renames? [no-rename? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]]
    (define git (find-executable-path "git"))
    (cond [(or (not (git-root)) (not git)) null]
          [else (let ([numstat-fold (git-numstat-make-fold (and day? s/day))]
                      [opt:log (list "log" git:%at "--numstat" "--no-merges" "--extended-regexp")]
                      [opt:n (if n (list (string-append "--max-count=" (number->string n))) null)]
                      [opt:since (if since (git-numstat-date-option "since" since localtime?) null)]
                      [opt:until (if until (git-numstat-date-option "until" until localtime?) null)]
                      [opt:rename (if no-rename? (list "--no-renames") null)]
                      [opt:author (for/list : (Listof String) ([a (if (list? authors) (in-list authors) (in-value authors))]) (string-append "--author=" a))]
                      [opt:committer (for/list : (Listof String) ([a (if (list? committers) (in-list committers) (in-value committers))]) (string-append "--committer=" a))])
                  (reverse ((inst fg-recon-exec* (Listof Git-Numstat))
                            #:silent git-silents 'git-numstat git (list opt:log opt:n opt:since opt:until opt:rename opt:author) numstat-fold
                            (if (and diff?)
                                ((inst fg-recon-exec* (Listof Git-Numstat))
                                 #:silent git-silents 'git-numstat git (list (list "diff" "--numstat") opt:rename) numstat-fold null)
                                null))))])))
