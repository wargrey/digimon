#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../exec.rkt"))

(require "renderer.rkt")

(require "../exec.rkt")
(require "../system.rkt")

(require "../../dtrace.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-exec : (-> Symbol Tex-Renderer Path-String Path-String Byte Boolean Path)
  (lambda [renderer latex TEXNAME.tex dest-dir retry on-error-logging]
    (define post-exec : (Option Tex-Post-Exec) (tex-renderer-post-exec latex))
    (define TEXNAME : Path (assert (file-name-from-path TEXNAME.tex) path?))
    (define TEXNAME.ext : Path (build-path dest-dir (path-replace-extension TEXNAME (tex-renderer-extension latex))))
    (define TEXNAME.log : Path (build-path dest-dir (path-replace-extension TEXNAME #".log")))
    (define log-timestamp : Natural (file-mtime TEXNAME.log))
    (define -output-directory : String (format "-output-directory=~a" dest-dir))
    
    (fg-recon-mkdir renderer dest-dir)

    ;;; NOTE: Scribble may generate resources in `(current-directory)`
    (parameterize ([current-directory dest-dir])
      (let rerun ([times : Natural 1])
        (fg-recon-exec (string->symbol (format "~a[~a]" renderer times))
                       (tex-renderer-program latex)
                       (list (list "-interaction=batchmode")
                             (list -output-directory)
                             (list (cond [(string? TEXNAME.tex) TEXNAME.tex]
                                         [else (path->string TEXNAME.tex)])))
                       digimon-system
                       (and on-error-logging
                            (tex-renderer-on-error-logging? latex)
                            (λ [[op : Symbol] [program : Path] [status : Nonnegative-Integer]]
                              (define log-now (file-mtime TEXNAME.log))
                              (cond [(<= log-now log-timestamp) (dtrace-warning #:topic op #:prefix? #true "log has not updated")]
                                    [(> log-now 0) (fg-recon-cat renderer TEXNAME.log (current-error-port))]))))
        
        (when (and (file-exists? TEXNAME.log) (> (file-mtime TEXNAME.log) log-timestamp))
          ;;; NOTE
          ;; see if we get a "Rerun" note, these seem to come in two flavors
          ;; * Label(s) may have changed. Rerun to get cross-references right.
          ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
          (when (call-with-input-file* TEXNAME.log
                  (lambda [[/dev/login : Input-Port]]
                    (regexp-match? #px#"changed\\.\\s+Rerun" /dev/login)))
            (cond [(>= times retry) (raise-user-error renderer "could not get a stable result after ~a runs" times)]
                  [else (rerun (+ times 1))])))))
    
    (cond [(not post-exec) TEXNAME.ext]
          [else (post-exec renderer TEXNAME.ext digimon-system)])))
