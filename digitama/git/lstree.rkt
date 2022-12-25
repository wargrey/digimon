#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/match)

(require "parameter.rkt")

(require "../../number.rkt")
(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct git-file
  ([pathname : String]
   [size : Natural]
   [permission : Index]
   [sha : Bytes])
  #:type-name Git-File
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-lstree-exec : (-> Path (Listof (Listof String)) (-> String (Listof Git-File) (Listof Git-File)) (Listof Git-File) (Listof Git-File))
  (lambda [git options tree-fold initial]
    ((inst fg-recon-exec* (Listof Git-File))
     #:silent git-silents (current-git-procedure)
     git options tree-fold initial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-file-make-fold : (-> (Option String) (-> String (Listof Git-File) (Listof Git-File)))
  (lambda [subpath/]
    (define filename-concat : (-> String String)
      (cond [(not subpath/) values]
            [else (λ [[fname : String]]
                    (string-append subpath/ fname))]))
    
    (λ [line files]
      (define tokens : (Listof String) (string-split line))
      
      (match (string-split line)
        [(list sperm type sha ssize file)
         (let ([size (string->natural ssize)])
           (cond [(not size) files] ; submodule path
                 [else (cons (git-file (filename-concat file) size
                                       (assert (string->index sperm 8))
                                       (string->bytes/utf-8 sha))
                             files)]))]
        [(list sperm type sha ssize file ...)
         (let ([size (string->natural ssize)])
           (cond [(not size) files] ; submodule path
                 [else (cons (git-file (filename-concat (regexp-replace px:spaced-fname:ls-tree line "\\1"))
                                       size (assert (string->index sperm 8)) (string->bytes/utf-8 sha))
                             files)]))]
        [deadcode files]))))
