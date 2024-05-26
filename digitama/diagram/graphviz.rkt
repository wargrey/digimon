#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/path)

(require "../exec.rkt")

(define-type GVSize (U Complex (Pairof Real Real) (List Real Real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gv-render : (->* ((U Path-String Bytes) Symbol)
                         (Symbol #:outfile (Option (U Path-String Output-Port))
                                 #:size (Option GVSize) #:min-size? Boolean
                                 #:stdin-log-level (Option Symbol))
                         Bytes)
  (lambda [#:outfile [outfile #false] #:size [size #false] #:min-size? [minsize? #false] #:stdin-log-level [log-level #false]
           src.gv target-lang [layout 'dot]]
    (define dot (find-executable-path "dot"))
   
    (cond [(not dot) (error layout "graphviz not found")]
          [(and (not (bytes? src.gv)) (not (file-exists? src.gv))) (error layout "no such DOT script file: ~a" src.gv)]
          [else (let ([operation 'graphviz])
                  (define options : (Listof (Listof String))
                    (list (list "-K" (symbol->immutable-string layout))
                          (list "-T" (symbol->immutable-string target-lang))
                          (if (not size) null (list (string-append "-Gsize=" (gv-size size minsize?))))
                          (cond [(string? src.gv) (list src.gv)]
                                [(path? src.gv) (list (path->string src.gv))]
                                [else null])))
                  
                  (when (or (string? outfile) (path? outfile))
                    (define dir (path-only outfile))
                    (when (and dir (not (directory-exists? dir)))
                      (fg-recon-mkdir operation dir)))

                  (let ([/dev/stdin (and (bytes? src.gv) (open-input-bytes src.gv))])
                    (if (or (path? outfile) (string? outfile))
                        (call-with-output-file* outfile #:exists 'truncate/replace
                          (λ [[/dev/dotout : Output-Port]] : Bytes
                            (fg-recon-exec/pipe #:stdin-log-level log-level
                                                #:/dev/stdin /dev/stdin
                                                #:/dev/stdout /dev/dotout
                                                operation dot options)))

                        (fg-recon-exec/pipe #:stdin-log-level log-level
                                            #:/dev/stdin /dev/stdin
                                            #:/dev/stdout outfile
                                            operation dot options))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gv-script-destination : (->* (Path-String Bytes) (Boolean #:dest-dirname String) (Option Path))
  (lambda [gv ext [contained-in-package? #true] #:dest-dirname [rootdir "diagram"]]
    (define dirname : (Option Path) (path-only gv))
    (define basename : (Option Path) (file-name-from-path gv))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     rootdir (path-replace-extension basename ext)))))

(define gv-size : (->* (GVSize) (Boolean) String)
  (lambda [dt [min? #false]]
    (define size : String
      (cond [(real? dt) (number->string (real->double-flonum dt))]
            [(list? dt) (string-append (number->string (real->double-flonum (car dt))) "," (number->string (real->double-flonum (cadr dt))))]
            [(pair? dt) (string-append (number->string (real->double-flonum (car dt))) "," (number->string (real->double-flonum (cdr dt))))]
            [else (string-append (number->string (real->double-flonum (real-part dt))) "," (number->string (real->double-flonum (imag-part dt))))]))

    (if (not min?) size (string-append size "!"))))
