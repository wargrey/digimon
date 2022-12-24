#lang typed/racket/base

(provide (all-defined-out))
(provide Git-Numstat Git-Numstat-Line)
(provide (struct-out Git-Language))

(require racket/path)
(require racket/port)

(require "dtrace.rkt")
(require "filesystem.rkt")

(require "digitama/git/numstat.rkt")
(require "digitama/git/langstat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-root : (->* () (Path-String) (Option Path))
  (lambda [[dir (current-directory)]]
    (define .git (build-path dir ".git"))
    (cond [(directory-exists? .git) (if (path? dir) dir (string->path dir))]
          [(file-exists? .git) (if (path? dir) dir (string->path dir))]
          [else (let-values ([(base name dir?) (split-path (simple-form-path dir))])
                  (and (path? base) (git-root base)))])))

(define git-submodule : (->* () (Path-String) (Listof String))
  (lambda [[dir (current-directory)]]
    (define git (find-executable-path "git"))
    (define rootdir : (Option Path) (git-root dir))
    (cond [(or (not rootdir) (not git)) null]
          [else (parameterize ([current-directory rootdir])
                  (git-submodule-list git))])))

(define git-numstat : (->* ()
                           (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean
                                        #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                        #:git-reverse? Boolean #:rkt-reverse? Boolean #:recursive? Boolean) 
                           (Listof Git-Numstat))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:git-reverse? [git-reverse? #false] #:rkt-reverse? [rkt-reverse? #true] #:recursive? [recursive? #true]
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
                            (define subrootdir : Path (build-path rootdir (path-normalize/system subname)))
                            (define submodpath : String (if (not subpath/) (string-append subname "/") (string-append subpath/ subname "/")))
                            
                            (git-numatat-merge stats (git-numstat-recursive subrootdir submodpath) git-reverse?))
                          (git-numstat rootdir subpath/))))

                  ((if rkt-reverse? reverse values)
                   (cond [(not recursive?) (git-numstat rootdir #false)]
                         [else (git-numstat-recursive rootdir #false)])))])))

(define git-langstat : (->* ()
                            (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean 
                                         #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                         #:grouping-option Git-Langstat-Grouping-Option #:types (Listof Symbol) #:reverse? Boolean #:recursive? Boolean)
                            (Immutable-HashTable Natural Git-Language))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #false]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:grouping-option [grouping-opt '(["Scribble" . #".scrbl"])] #:types [types null] #:reverse? [reverse? #false] #:recursive? [recursive? #true]
           [dir (current-directory)]]
    (parameterize ([current-git-procedure git-langstat])
      (define numstats : (Listof Git-Numstat)
        (git-numstat #:no-renames? no-renames? #:with-diff? diff? #:authors authors #:committers committers
                     #:group-by-day? day? #:since since #:until until #:localtime? localtime? #:n n
                     #:git-reverse? reverse? #:rkt-reverse? #false #:recursive? recursive?
                     dir))

      (git-numstats->langstat numstats types grouping-opt))))

(git-langstat #:since -30 "/Users/wargrey/Laboratory/YouthLanguage/racket/digitama/big-bang")
