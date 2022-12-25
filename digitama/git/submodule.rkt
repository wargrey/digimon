#lang typed/racket/base

(provide (all-defined-out))

(require "parameter.rkt")

(require "../exec.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Match-Datum (U Regexp Byte-Regexp String (Listof (U Regexp Byte-Regexp String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-list : (-> Path (Listof String))
  (lambda [git]
    (reverse
     ((inst fg-recon-exec* (Listof String))
      #:silent git-silents (current-git-procedure)
      git (list (list "submodule" "status"))
      git-submodule-path-fold null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-path-fold : (-> String (Listof String) (Listof String))
  (lambda [line paths]
    (define maybe-path (regexp-match px:submodule:status line))
    
    (cond [(not maybe-path) paths]
          [else (cons (assert (cadr maybe-path)) paths)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-ignore? : (-> String Git-Match-Datum Boolean)
  (lambda [name pattern]
    (cond [(regexp? pattern) (regexp-match? pattern name)]
          [(string? pattern) (string=? pattern name)]
          [(byte-regexp? pattern) (regexp-match? pattern name)]
          [else (for/or : Boolean ([p (in-list pattern)])
                  (git-submodule-ignore? name p))])))

(define git-submodule-rootdir-concat : (-> Path String Path)
  (lambda [rootdir subname]
    (build-path rootdir (path-normalize/system subname))))

(define git-submodule-path-concat : (-> (Option String) String String)
  (lambda [subpath/ subname]
    (cond [(not subpath/) (string-append subname "/")]
          [else (string-append subpath/ subname "/")])))
