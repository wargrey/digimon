#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "zipinfo.rkt")
(require "deflate.rkt")
(require "archive.rkt")

(require "../../port.rkt")

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
    (define ?zip (regexp-match #px"[^.]+$" (archive-port-name /dev/zipin)))
    (define port-name (format "~a://~a" (if (not ?zip) 'zip (car ?zip)) (zip-directory-filename cdir)))

    (file-position /dev/zipin (zip-directory-relative-offset cdir))
    (read-zip-entry /dev/zipin) ; for the sake of efficiency, no further validity check for entries here.
    
    (case (zip-directory-compression cdir)
      [(deflated) (open-input-deflated-block /dev/zipin (zip-directory-csize cdir) #false #:name port-name #:error-name 'zip:deflated)]
      [else (open-input-block /dev/zipin (zip-directory-csize cdir) #false #:name port-name)])))
