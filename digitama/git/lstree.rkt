#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/match)

(require "parameter.rkt")

(require "../../number.rkt")
(require "../../filesystem.rkt")
(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct git-file
  ([unix-path : String]
   [size : Natural]
   [permission : Index]
   [sha : Bytes])
  #:type-name Git-File
  #:transparent)

(define make-git-file : (->* (Path-String) (#:size Natural #:permission Index #:sha Bytes) Git-File)
  (lambda [pathname #:size [size 0] #:permission [permission 0] #:sha [sha #""]]
    (git-file (path->string (path-normalize/system pathname 'unix))
              size permission sha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-lstree-exec : (-> Path (Listof (Listof String)) (-> String (Listof Git-File) (Listof Git-File)) (Listof Git-File) (Listof Git-File))
  (lambda [git options tree-fold initial]
    ((inst fg-recon-exec* (Listof Git-File))
     #:silent git-silents (current-git-procedure)
     git options tree-fold initial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-file-make-fold : (-> (Option String) Git-Match-Datum (-> String (Listof Git-File) (Listof Git-File)))
  (lambda [subpath/ filter]
    (define filename-concat : (-> String String)
      (cond [(not subpath/) values]
            [else (λ [[fname : String]]
                    (string-append subpath/ fname))]))
    
    (λ [line files]
      (define tokens : (Listof String) (string-split line))
      
      (match (string-split line)
        [(list sperm type sha ssize file)
         (let ([size (string->natural ssize)]
               [full-pathname (filename-concat file)])
           (cond [(not size) files] ; submodule path
                 [(not (git-path-match? full-pathname filter #:match-for-empty? #true)) files]
                 [else (cons (git-file full-pathname size (assert (string->index sperm 8)) (string->bytes/utf-8 sha))
                             files)]))]
        [(list sperm type sha ssize file ...)
         (let ([size (string->natural ssize)]
               [full-pathname (filename-concat (regexp-replace px:spaced-fname:ls-tree line "\\1"))])
           (cond [(not size) files] ; submodule path
                 [(not (git-path-match? full-pathname filter #:match-for-empty? #true)) files]
                 [else (cons (git-file full-pathname size (assert (string->index sperm 8)) (string->bytes/utf-8 sha))
                             files)]))]
        [deadcode files]))))
