#lang typed/racket/base

(require racket/path)

(require "../tex.rkt")
(require "../renderer.rkt")

(require "../../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define euptex-preamble-filter : Tex-Preamble-Filter
  (lambda [line status]
    (cond [(regexp-match? #px"\\\\usepackage\\[utf8\\][{]inputenc[}]" line) (values "\\usepackage{xeCJK}" 'used)]
          [(regexp-match? #px"\\\\newcommand[{]\\\\packageCJK[}]" line) (values #false 'commandset)]
          [(regexp-match? #px"\\\\packageCJK" line) (values #false 'EOF)]
          [else (values line status)])))

(define euptex-post-exec : Tex-Post-Exec
  (lambda [func-name TEXNAME.pdf system]
    (define dvipdf : Symbol 'xdvipdfmx)
    (define TEXNAME.dvi : Path (path-replace-extension TEXNAME.pdf #".dvi"))
    (define /bin/dvipdf : (Option Path) (tex-find-binary-path dvipdf))
    
    (cond [(not /bin/dvipdf) (raise-user-error func-name "cannot find `~a`" dvipdf)]
          [else (parameterize ([current-directory (or (path-only TEXNAME.pdf) (current-directory))])
                  (fg-exec func-name /bin/dvipdf (list (list (path->string TEXNAME.dvi))) system #:silent 'stderr))])

    TEXNAME.pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tex-register-renderer 'uplatex #:filter #false #:post-exec euptex-post-exec)
