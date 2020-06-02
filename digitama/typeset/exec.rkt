#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/port)

(require "renderer.rkt")

(require "../exec.rkt")
(require "../system.rkt")

(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-exec : (-> Symbol Tex-Renderer Path-String Path-String Byte Path)
  (lambda [renderer latex TEXNAME.tex dest-dir retry]
    (define post-exec : (Option Tex-Post-Exec) (tex-renderer-post-exec latex))
    (define TEXNAME : Path (assert (file-name-from-path TEXNAME.tex) path?))
    (define TEXNAME.ext : Path (build-path dest-dir (path-replace-extension TEXNAME (tex-renderer-extension latex))))
    (define TEXNAME.log : Path (build-path dest-dir (path-replace-extension TEXNAME #".log")))
    (define log-timestamp : Natural (file-mtime TEXNAME.log))
    (define -output-directory : String (format "-output-directory=~a" dest-dir))
    
    (make-directory* dest-dir)

    ;;; NOTE: Scribble may generate resources in `(current-directory)`
    (parameterize ([current-directory dest-dir])
      (let rerun ([times : Natural 1])
        (fg-exec (string->symbol (format "~a[~a]" renderer times))
                 (tex-renderer-program latex)
                 (list (list "-interaction=batchmode")
                       (list -output-directory)
                       (list (cond [(string? TEXNAME.tex) TEXNAME.tex]
                                   [else (path->string TEXNAME.tex)])))
                 digimon-system
                 (and (tex-renderer-on-error-logging? latex)
                      (λ [[op : Symbol] [program : Path] [status : Nonnegative-Integer]]
                        (define log-now (file-mtime TEXNAME.log))
                        (when (> log-now 0)
                          (log-message (current-logger) 'warning op (format "cat ~a" TEXNAME.log) #false)
                          (call-with-input-file* TEXNAME.log
                            (λ [[/dev/login : Input-Port]]
                              (copy-port /dev/login (current-error-port)))))
                        (when (<= log-now log-timestamp)
                          (log-message (current-logger) 'warning op "log has not updated" #false)))))
        
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
