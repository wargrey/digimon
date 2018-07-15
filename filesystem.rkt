#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/path))

(require racket/path)

(define dirname : (-> Path-String [#:rootname String] String)
  (lambda [path #:rootname [root "/"]]
    (define dir : (Option Path-String)
      (let ([dir : Path (simple-form-path path)])
        (cond [(directory-exists? path) path]
              [else (path-only path)])))
    (cond [(not dir) #| this should not happen |# root]
          [else (let-values ([(_b name _?) (split-path dir)])
                  (cond [(path? name) (path->string name)]
                        [else root]))])))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))
