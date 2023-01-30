#lang typed/racket/base

(provide (all-defined-out))
(provide git-default-subgroups git-files->langfiles git-files->langsizes)
(provide git-numstats->langstats git-numstats->additions+deletions* git-langstats->additions+deletions*)
(provide Git-Numstat Git-Numstat-Line Git-Match-Datum)
(provide (struct-out Git-Language-With) (struct-out Git-File))

(require "dtrace.rkt")
(require "filesystem.rkt")

(require "digitama/git/numstat.rkt")
(require "digitama/git/langstat.rkt")
(require "digitama/git/lstree.rkt")
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
             (parameterize ([current-git-procedure git-submodules])
               (cond [(not recursive?) (map (inst car String String) (git-submodule-list git))]
                     [else (let list-submodule : (Option (Listof String)) ([rootdir : Path rootdir]
                                                                           [subpath/ : (Option String) #false])
                             (dtrace-note #:topic (current-git-procedure) "cd ~a" rootdir)
                             
                             (and (file-exists? (build-path rootdir ".gitmodules"))
                                  (parameterize ([current-directory rootdir])
                                    (let ([submodules (git-submodule-list git)])
                                      (for/fold ([submodules : (Listof String) (for/list ([subname (in-list submodules)])
                                                                                 (git-submodule-path-concat subpath/ (car subname)))])
                                                ([subname (in-list submodules)])
                                        (define subrootdir : Path (git-submodule-rootdir-concat rootdir (car subname)))
                                        (define submodpath : String (git-submodule-path-concat subpath/ (car subname)))
                                        (define subpaths : (Option (Listof String)) (list-submodule subrootdir submodpath))
                                        
                                        (cond [(or (not subpaths) (null? subpaths)) submodules]
                                              [else (append submodules subpaths)]))))))])))
        null)))

(define git-list-tree : (->* ()
                             (Path-String #:treeish String #:reverse? Boolean #:recursive? Boolean
                                          #:ignore-submodule Git-Match-Datum #:filter Git-Match-Datum)
                             (Listof Git-File))
  ; TODO: work with `git status --short`
  (lambda [[dir (current-directory)] #:treeish [treeish "HEAD"] #:reverse? [reverse? #false] #:recursive? [recursive? #true]
                                     #:ignore-submodule [ignore null] #:filter [filter null]]
    (define git (find-executable-path "git"))
    (define rootdir : (Option Path) (git-root dir))
    (or (and rootdir git
             (parameterize ([current-git-procedure (or (current-git-procedure) git-list-tree)])
               (let ([opt:lstree (list "ls-tree" "--full-tree" "--long" "-r")])
                 (define (git-lstree [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-File)
                   (parameterize ([current-directory rootdir])
                     (git-lstree-exec git (list opt:lstree (list treeish))
                                      (git-file-make-fold subpath/ filter) null)))
                 
                 (define (git-lstree-recursive [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-File)
                   (parameterize ([current-directory rootdir])
                     (dtrace-note #:topic (current-git-procedure) "cd ~a" rootdir)
                     
                     (if (file-exists? (build-path rootdir ".gitmodules"))
                         (for/fold ([files : (Listof Git-File) (git-lstree rootdir subpath/)])
                                   ([subname (in-list (git-submodule-list git))])
                           (define subrootdir : Path (git-submodule-rootdir-concat rootdir (car subname)))
                           (define submodpath : String (git-submodule-path-concat subpath/ (car subname)))
                           
                           (cond [(git-path-match? submodpath ignore #:match-for-empty? #false) files]
                                 [else (append (git-lstree-recursive subrootdir submodpath) files)]))
                         (git-lstree rootdir subpath/))))
                 
                 ((if reverse? reverse values)
                  (cond [(not recursive?) (git-lstree rootdir #false)]
                        [else (git-lstree-recursive rootdir #false)])))))
        null)))

(define git-list-langtree : (->* ()
                                 (Path-String #:treeish String #:types (Listof Symbol)#:grouping-option Git-Langstat-Grouping-Option
                                              #:recursive? Boolean #:ignore-submodule Git-Match-Datum #:filter Git-Match-Datum)
                                 (Immutable-HashTable Index (Git-Language-With (Listof Git-File))))
  (lambda [#:treeish [treeish "HEAD"] #:types [types null] #:grouping-option [grouping-opt git-default-subgroups]
           #:recursive? [recursive? #true] #:ignore-submodule [ignore null] #:filter [filter null]
           [dir (current-directory)]]
    (parameterize ([current-git-procedure git-list-langtree])
      (define files : (Listof Git-File) (git-list-tree dir #:treeish treeish #:recursive? recursive? #:ignore-submodule ignore #:filter filter))

      (git-files->langfiles files types grouping-opt))))

(define git-list-langsize : (->* ()
                                 (Path-String #:treeish String #:types (Listof Symbol)#:grouping-option Git-Langstat-Grouping-Option
                                              #:recursive? Boolean #:ignore-submodule Git-Match-Datum #:filter Git-Match-Datum)
                                 (Immutable-HashTable Index (Git-Language-With Natural)))
  (lambda [#:treeish [treeish "HEAD"] #:types [types null] #:grouping-option [grouping-opt git-default-subgroups]
           #:recursive? [recursive? #true] #:ignore-submodule [ignore null] #:filter [filter null]
           [dir (current-directory)]]
    (parameterize ([current-git-procedure git-list-langsize])
      (define files : (Listof Git-File) (git-list-tree dir #:treeish treeish #:recursive? recursive? #:ignore-submodule ignore #:filter filter))

      (git-files->langsizes files types grouping-opt))))

(define git-numstat : (->* ()
                           (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean
                                        #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                        #:git-reverse? Boolean #:rkt-reverse? Boolean #:recursive? Boolean #:ignore-submodule Git-Match-Datum #:filter Git-Match-Datum)
                           (Listof Git-Numstat))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #true]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:git-reverse? [git-reverse? #false] #:rkt-reverse? [rkt-reverse? #true] #:recursive? [recursive? #true] #:ignore-submodule [ignore null] #:filter [filter null]
           [dir (current-directory)]]
    (define git (find-executable-path "git"))
    (define rootdir : (Option Path) (git-root dir))
    (cond [(or (not rootdir) (not git)) null]
          [else (parameterize ([current-git-procedure (or (current-git-procedure) git-numstat)])
                  (let ([opt:log (list "log" git:%at "--numstat" "--no-merges" "--extended-regexp")]
                        [opt:n (if n (list (string-append "--max-count=" (number->string n))) null)]
                        [opt:since (if since (git-numstat-date-option "since" since localtime?) null)]
                        [opt:until (if until (git-numstat-date-option "until" until localtime?) null)]
                        [opt:reverse (if git-reverse? (list "--reverse") null)]
                        [opt:rename (if no-renames? (list "--no-renames") null)]
                        [opt:author (for/list : (Listof String) ([a (if (list? authors) (in-list authors) (in-value authors))]) (string-append "--author=" a))]
                        [opt:committer (for/list : (Listof String) ([a (if (list? committers) (in-list committers) (in-value committers))]) (string-append "--committer=" a))])
                    
                    (define (git-numstat [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-Numstat)
                      (parameterize ([current-directory rootdir])
                        (define numstat-fold (git-numstat-make-fold (and day? s/day) subpath/ filter))
                        
                        (git-numstat-exec git (list opt:log opt:n opt:since opt:until opt:reverse opt:rename opt:author opt:committer) numstat-fold
                                          (cond [(and diff?) (git-numstat-exec git (list (list "diff" "--numstat") opt:rename) numstat-fold null)]
                                                [else null]))))
                    
                    (define (git-numstat-recursive [rootdir : Path] [subpath/ : (Option String)]) : (Listof Git-Numstat)
                      (parameterize ([current-directory rootdir])
                        (dtrace-note #:topic (current-git-procedure) "cd ~a" rootdir)
                        
                        (if (file-exists? (build-path rootdir ".gitmodules"))
                            (for/fold ([stats : (Listof Git-Numstat) (git-numstat rootdir subpath/)])
                                      ([subname (in-list (git-submodule-list git))])
                              (define subrootdir : Path (git-submodule-rootdir-concat rootdir (car subname)))
                              (define submodpath : String (git-submodule-path-concat subpath/ (car subname)))
                              
                              (cond [(git-path-match? submodpath ignore #:match-for-empty? #false) stats]
                                    [else (let ([subnumstats (git-numstat-recursive subrootdir submodpath)])
                                            (git-numatat-merge stats subnumstats git-reverse?))]))
                            (git-numstat rootdir subpath/))))
                    
                    ((if rkt-reverse? reverse values)
                     (cond [(not recursive?) (git-numstat rootdir #false)]
                           [else (git-numstat-recursive rootdir #false)]))))])))

(define git-langstat : (->* ()
                            (Path-String #:group-by-day? Boolean #:no-renames? Boolean #:since (Option Git-Date-Datum) #:until (Option Git-Date-Datum) #:localtime? Boolean 
                                         #:n (Option Natural) #:authors (U String (Listof String)) #:committers (U String (Listof String)) #:with-diff? Boolean
                                         #:grouping-option Git-Langstat-Grouping-Option #:types (Listof Symbol) #:reverse? Boolean #:recursive? Boolean
                                         #:ignore-submodule Git-Match-Datum #:filter Git-Match-Datum)
                            (Immutable-HashTable Index (Git-Language-With (Listof Git-Numstat))))
  (lambda [#:no-renames? [no-renames? #false] #:group-by-day? [day? #true] #:since [since #false] #:until [until #false] #:localtime? [localtime? #true]
           #:n [n #false] #:authors [authors null] #:committers [committers null] #:with-diff? [diff? (and (null? authors) (null? committers) (not until))]
           #:grouping-option [grouping-opt git-default-subgroups] #:types [types null] #:reverse? [reverse? #false] #:recursive? [recursive? #true]
           #:ignore-submodule [ignore null] #:filter [filter null]
           [dir (current-directory)]]
    (parameterize ([current-git-procedure git-langstat])
      (define numstats : (Listof Git-Numstat)
        (git-numstat #:no-renames? no-renames? #:with-diff? diff? #:authors authors #:committers committers
                     #:group-by-day? day? #:since since #:until until #:localtime? localtime? #:n n
                     #:recursive? recursive? #:ignore-submodule ignore #:filter filter
                     #:git-reverse? reverse? #:rkt-reverse? #false
                     dir))

      (git-numstats->langstats numstats types grouping-opt))))
