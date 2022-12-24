#lang typed/racket/base

(provide (all-defined-out))
(provide Git-Numstat Git-Numstat-Line Git-Match-Datum)
(provide (struct-out Git-Language))

(require racket/path)
(require racket/port)

(require "dtrace.rkt")
(require "filesystem.rkt")

(require "digitama/git/numstat.rkt")
(require "digitama/git/langstat.rkt")
(require "digitama/git/submodule.rkt")

(require "digitama/git/parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-root : (->* () (Path-String) (Option Path))
  (lambda [[dir (current-directory)]]
    (define .git (build-path dir ".git"))
    (cond [(not (or (directory-exists? .git) (file-exists? .git)))
           (let-values ([(base name dir?) (split-path (simple-form-path dir))])
             (and (path? base) (git-root base)))]
          [(string? dir) (string->path dir)]
          [else dir])))

(define git-submodules : (->* () (Path-String #:recursive? Boolean) (Listof String))
  (lambda [[dir (current-directory)] #:recursive? [recursive? #true]]
    (define git (find-executable-path "git"))
    (define rootdir : (Option Path) (git-root dir))
    (or (and rootdir git
             (cond [(not recursive?) (git-submodule-list git)]
                   [else (let list-submodule : (Option (Listof String)) ([rootdir : Path rootdir]
                                                                         [subpath/ : (Option String) #false])
                           (dtrace-note #:topic (current-git-procedure) "cd ~a" rootdir)
                           
                           (and (file-exists? (build-path rootdir ".gitmodules"))
                                (parameterize ([current-directory rootdir])
                                  (let ([submodules (git-submodule-list git)])
                                    (for/fold ([submodules : (Listof String) (for/list ([subname (in-list submodules)])
                                                                               (git-submodule-path-concat subpath/ subname))])
                                              ([subname (in-list submodules)])
                                      (define subrootdir : Path (git-submodule-rootdir-concat rootdir subname))
                                      (define submodpath : String (git-submodule-path-concat subpath/ subname))
                                      (define subpaths : (Option (Listof String)) (list-submodule subrootdir submodpath))

                                      (cond [(or (not subpaths) (null? subpaths)) submodules]
                                            [else (append submodules subpaths)]))))))]))
        null)))

(define git-numstat : (->* ()
                           (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean
                                        #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                        #:git-reverse? Boolean #:rkt-reverse? Boolean #:recursive? Boolean #:ignore-submodule Git-Match-Datum)
                           (Listof Git-Numstat))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:git-reverse? [git-reverse? #false] #:rkt-reverse? [rkt-reverse? #true] #:recursive? [recursive? #true] #:ignore-submodule [ignore null]
           [dir (current-directory)]]
    (define git (find-executable-path "git"))
    (define rootdir : (Option Path) (git-root dir))
    (cond [(or (not rootdir) (not git)) null]
          [else (let ([opt:log (list "log" git:%at "--numstat" "--no-merges" "--extended-regexp")]
                      [opt:n (if n (list (string-append "--max-count=" (number->string n))) null)]
                      [opt:since (if since (git-numstat-date-option "since" since localtime?) null)]
                      [opt:until (if until (git-numstat-date-option "until" until localtime?) null)]
                      [opt:reverse (if git-reverse? (list "--reverse") null)]
                      [opt:rename (if no-renames? (list "--no-renames") null)]
                      [opt:author (for/list : (Listof String) ([a (if (list? authors) (in-list authors) (in-value authors))]) (string-append "--author=" a))]
                      [opt:committer (for/list : (Listof String) ([a (if (list? committers) (in-list committers) (in-value committers))]) (string-append "--committer=" a))])

                  (define (git-numstat [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-Numstat)
                    (parameterize ([current-directory rootdir])
                      (define numstat-fold (git-numstat-make-fold (and day? s/day) subpath/))
                      
                      (git-numstat-exec git (list opt:log opt:n opt:since opt:until opt:reverse opt:rename opt:author opt:committer) numstat-fold
                                        (cond [(and diff?) (git-numstat-exec git (list (list "diff" "--numstat") opt:rename) numstat-fold null)]
                                              [else null]))))

                  (define (git-numstat-recursive [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-Numstat)
                    (parameterize ([current-directory rootdir])
                      (dtrace-note #:topic (current-git-procedure) "cd ~a" rootdir)

                      (if (file-exists? (build-path rootdir ".gitmodules"))
                          (for/fold ([stats : (Listof Git-Numstat) (git-numstat rootdir subpath/)])
                                    ([subname (in-list (git-submodule-list git))])
                            (define subrootdir : Path (git-submodule-rootdir-concat rootdir subname))
                            (define submodpath : String (git-submodule-path-concat subpath/ subname))

                            (cond [(git-numstat-ignore? submodpath ignore) stats]
                                  [else (let ([subnumstats (git-numstat-recursive subrootdir submodpath)])
                                          (git-numatat-merge stats subnumstats git-reverse?))]))
                          (git-numstat rootdir subpath/))))

                  ((if rkt-reverse? reverse values)
                   (cond [(not recursive?) (git-numstat rootdir #false)]
                         [else (git-numstat-recursive rootdir #false)])))])))

(define git-langstat : (->* ()
                            (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean 
                                         #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                         #:grouping-option Git-Langstat-Grouping-Option #:types (Listof Symbol) #:reverse? Boolean #:recursive? Boolean
                                         #:ignore-submodule Git-Match-Datum)
                            (Immutable-HashTable Natural Git-Language))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:grouping-option [grouping-opt '(["Scribble" . #".scrbl"])] #:types [types null] #:reverse? [reverse? #false] #:recursive? [recursive? #true]
           #:ignore-submodule [ignore null]
           [dir (current-directory)]]
    (parameterize ([current-git-procedure git-langstat])
      (define numstats : (Listof Git-Numstat)
        (git-numstat #:no-renames? no-renames? #:with-diff? diff? #:authors authors #:committers committers
                     #:group-by-day? day? #:since since #:until until #:localtime? localtime? #:n n
                     #:recursive? recursive? #:ignore-submodule ignore #:git-reverse? reverse? #:rkt-reverse? #false
                     dir))

      (git-numstats->langstat numstats types grouping-opt))))
