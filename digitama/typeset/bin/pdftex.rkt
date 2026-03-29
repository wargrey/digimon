#lang typed/racket/base

(require racket/path)

(require "../engine.rkt")

(require "../../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex-post-exec : Tex-Post-Exec
  (lambda [func-name TEXNAME.pdf]
    (define-values (dvips topdf) (values 'dvips 'dvipdfmx))
    (define TEXNAME.dvi : Path (path-replace-extension TEXNAME.pdf #".dvi"))
    ;(define TEXNAME.ps : Path (path-replace-extension TEXNAME.pdf #".ps"))
    ;(define /bin/dvips : (Option Path) (tex-find-binary-path dvips))
    (define /bin/topdf : (Option Path) (tex-find-binary-path topdf))

    (cond ;[(not /bin/dvips)  (raise-user-error func-name "cannot find `~a`" dvips)]
          [(not /bin/topdf) (raise-user-error func-name "cannot find `~a`" topdf)]
          [else (parameterize ([current-directory (or (path-only TEXNAME.pdf) (current-directory))])
                  ;(fg-recon-exec func-name /bin/dvips (list (list (path->string TEXNAME.dvi))) #:silent '(stderr))
                  (fg-recon-exec func-name /bin/topdf (list (list (path->string TEXNAME.dvi))) #:silent '(stderr)))])

    TEXNAME.pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (require "../tex.rkt")
  
  (tex-register-engine 'latex #:draftmode "-draftmode" #:post-exec latex-post-exec)
  (tex-register-engine 'pdflatex #:draftmode "-draftmode"))
