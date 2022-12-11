#lang typed/racket/base

(require racket/path)

(require "../tex.rkt")
(require "../engine.rkt")

(require "../../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define euptex-post-exec : Tex-Post-Exec
  (lambda [func-name TEXNAME.pdf system]
    (define dvipdf : Symbol 'xdvipdfmx)
    (define TEXNAME.dvi : Path (path-replace-extension TEXNAME.pdf #".dvi"))
    (define /bin/dvipdf : (Option Path) (tex-find-binary-path dvipdf))
    
    (cond [(not /bin/dvipdf) (raise-user-error func-name "cannot find `~a`" dvipdf)]
          [else (parameterize ([current-directory (or (path-only TEXNAME.pdf) (current-directory))])
                  (fg-recon-exec #:silent '(stderr)
                                 func-name /bin/dvipdf (list (list (path->string TEXNAME.dvi))) system))])

    TEXNAME.pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (tex-register-engine 'uplatex #:post-exec euptex-post-exec))
