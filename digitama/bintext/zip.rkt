#lang typed/racket/base

(provide (all-defined-out))

(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-path-normalize : (-> Path-String String)
  (let ([pxwin-separator #rx"\\\\"])
    (lambda [filename]
      (cond [(string? filename) (regexp-replace* pxwin-separator filename "/")]
            [else (case (path-convention-type filename)
                    [(windows) (regexp-replace* pxwin-separator (path->string filename) "/")]
                    [else (path->string filename)])]))))

(define zip-folder-entry? : (-> ZIP-Directory Boolean)
  (lambda [cdir]
    (let ([fname (zip-directory-filename cdir)])
      (eq? (string-ref fname (sub1 (string-length fname))) #\/))))

(define open-input-zip-entry : (-> Input-Port ZIP-Directory Input-Port)
  (lambda [/dev/zipin cdir]
    /dev/zipin))
