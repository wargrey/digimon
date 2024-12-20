#lang typed/racket/base

(provide (all-defined-out))

(require "parameter.rkt")

(require "../exec.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-list : (-> Path (Listof (Pairof String String)))
  (lambda [git]
    (reverse
     ((inst fg-recon-exec* (Listof (Pairof String String)))
      #:silent git-silents (current-git-procedure)
      git (list (list "submodule" "status"))
      git-submodule-path-fold null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-path-fold : (-> String (Listof (Pairof String String)) (Listof (Pairof String String)))
  (lambda [line paths]
    (define modline (regexp-match px:submodule:status line))

    ; TODO: How to deal with submodules referring to the same repository

    (or (and modline
             (let ([modsha (cadr modline)]
                   [modpath (caddr modline)])
               (and modsha modpath
                    (not (member modsha (map (inst cdr String String) paths)))
                    (cons (cons modpath modsha) paths))))
        paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-submodule-rootdir-concat : (-> Path String Path)
  (lambda [rootdir subname]
    (build-path rootdir (path-normalize/system subname))))

(define git-submodule-path-concat : (-> (Option String) String String)
  (lambda [subpath/ subname]
    (cond [(not subpath/) (string-append subname "/")]
          [else (string-append subpath/ subname "/")])))
