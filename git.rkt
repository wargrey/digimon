#lang typed/racket/base

(provide (all-defined-out))
(provide Git-Numstat Git-Numstat-Line)
(provide (struct-out Git-Language))

(require racket/path)
(require racket/port)

(require "digitama/git/numstat.rkt")
(require "digitama/git/langstat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-root : (->* () (Path-String) (Option Path-String))
  (lambda [[dir (current-directory)]]
    (define .git (build-path dir ".git"))
    (cond [(directory-exists? .git) .git]
          [else (let-values ([(base name dir?) (split-path (simple-form-path dir))])
                  (and (path? base) (git-root base)))])))

(define git-numstat : (-> [#:group-by-day? Boolean] [#:no-renames? Boolean] [#:since (Option Git-Date-Datum)] [#:until (Option Git-Date-Datum)] [#:localtime? Boolean] 
                          [#:n (Option Natural)] [#:authors (U String (Listof String))] [#:committers (U String (Listof String))] [#:with-diff? Boolean]
                          [#:git-reverse? Boolean] [#:rkt-reverse? Boolean]
                          (Listof Git-Numstat))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:git-reverse? [git-reverse? #false] #:rkt-reverse? [rkt-reverse? #true]]
    (define git (find-executable-path "git"))
    (cond [(or (not (git-root)) (not git)) null]
          [else (let ([numstat-fold (git-numstat-make-fold (and day? s/day))]
                      [opt:log (list "log" git:%at "--numstat" "--no-merges" "--extended-regexp")]
                      [opt:n (if n (list (string-append "--max-count=" (number->string n))) null)]
                      [opt:since (if since (git-numstat-date-option "since" since localtime?) null)]
                      [opt:until (if until (git-numstat-date-option "until" until localtime?) null)]
                      [opt:reverse (if git-reverse? (list "--reverse") null)]
                      [opt:rename (if no-renames? (list "--no-renames") null)]
                      [opt:author (for/list : (Listof String) ([a (if (list? authors) (in-list authors) (in-value authors))]) (string-append "--author=" a))]
                      [opt:committer (for/list : (Listof String) ([a (if (list? committers) (in-list committers) (in-value committers))]) (string-append "--committer=" a))])
                  ((if rkt-reverse? reverse values)
                   (git-numstat-exec git (list opt:log opt:n opt:since opt:until opt:reverse opt:rename opt:author opt:committer) numstat-fold
                                     (cond [(and diff?) (git-numstat-exec git (list (list "diff" "--numstat") opt:rename) numstat-fold null)]
                                           [else null]))))])))

(define git-langstat : (-> [#:group-by-day? Boolean] [#:no-renames? Boolean] [#:since (Option Git-Date-Datum)] [#:until (Option Git-Date-Datum)] [#:localtime? Boolean] 
                           [#:n (Option Natural)] [#:authors (U String (Listof String))] [#:committers (U String (Listof String))] [#:with-diff? Boolean]
                           [#:grouping-option Git-Langstat-Grouping-Option] [#:types (Listof Symbol)] [#:reverse? Boolean]
                           (Immutable-HashTable Natural Git-Language))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:grouping-option [grouping-opt '(["Scrbble" . #".scrbl"])] #:types [types null] #:reverse? [reverse? #false]]
    (parameterize ([current-git-procedure git-langstat])
      (define numstats : (Listof Git-Numstat)
        (git-numstat #:no-renames? no-renames? #:with-diff? diff? #:authors authors #:committers committers
                     #:group-by-day? day? #:since since #:until until #:localtime? localtime? #:n n
                     #:git-reverse? reverse? #:rkt-reverse? #false))

      (git-numstats->langstat numstats types grouping-opt))))
