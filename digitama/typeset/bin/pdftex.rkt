#lang typed/racket/base

(require racket/path)

(require "../tex.rkt")
(require "../renderer.rkt")

(require "../../exec.rkt")
(require "../../../echo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex-post-exec : Tex-Post-Exec
  (lambda [func-name TEXNAME.pdf system]
    (define TEXNAME.dvi : Path (path-replace-extension TEXNAME.pdf #".dvi"))
    (define TEXNAME.ps : Path (path-replace-extension TEXNAME.pdf #".ps"))
    (define dvips : (Option Path) (tex-find-binary-path 'dvips))
    (define ps2pdf : (Option Path) (tex-find-binary-path 'pstopdf))

    (cond [(not dvips)  (raise-user-error func-name "cannot find `dvips`")]
          [(not ps2pdf) (raise-user-error func-name "cannot find `pstopdf`")]
          [else (parameterize ([current-directory (or (path-only TEXNAME.pdf) (current-directory))])
                  (fg-exec func-name dvips (list (list (path->string TEXNAME.dvi))) system #:silent 'stderr)
                  (fg-exec func-name ps2pdf (list (list (path->string TEXNAME.ps))) system #:silent 'stderr))])

    TEXNAME.pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tex-register-renderer 'latex #:post-exec latex-post-exec)
(tex-register-renderer 'pdflatex)
