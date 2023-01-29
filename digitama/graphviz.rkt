#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require "../filesystem.rkt")

(require "exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gv-render : (->* (Path-String Symbol) (Symbol #:outfile (Option (U Path-String Output-Port))) Bytes)
  (lambda [src.gv target-lang [layout 'dot] #:outfile [outfile #false]]
    (define dot (find-executable-path "dot"))
   
    (cond [(not dot) (error layout "graphviz not found")]
          [(not (file-exists? src.gv)) (error layout "no such tex file: ~a" src.gv)]
          [else (let ([operation 'graphviz])
                  (define options : (Listof (Listof String))
                    (list (list "-K" (symbol->immutable-string layout))
                          (list "-T" (symbol->immutable-string target-lang))
                          (list (if (string? src.gv) src.gv (path->string src.gv)))))
                  
                  (when (or (string? outfile) (path? outfile))
                    (define dir (path-only outfile))
                    (when (and dir (not (directory-exists? dir)))
                      (fg-recon-mkdir operation dir)))

                  (let ([dst-bytes (fg-recon-exec/pipe operation dot options)])
                    (unless (not outfile)
                      (if (output-port? outfile)
                          (write-bytes dst-bytes outfile)
                          (call-with-output-file* outfile #:exists 'truncate/replace
                            (λ [[/dev/dotout : Output-Port]] : Index
                              (write-bytes dst-bytes /dev/dotout)))))
                    dst-bytes))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gv-script-destination : (->* (Path-String Bytes) (Boolean #:dest-dirname String) (Option Path))
  (lambda [gv ext [contained-in-package? #true] #:dest-dirname [rootdir "graphviz"]]
    (define dirname : (Option Path) (path-only gv))
    (define basename : (Option Path) (file-name-from-path gv))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     rootdir (path-replace-extension basename ext)))))
