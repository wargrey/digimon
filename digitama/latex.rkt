#lang typed/racket/base

(provide (all-defined-out))
(provide tex-renderer Tex-Renderer)

(require racket/path)
(require racket/file)

(require/typed
 racket/base
 [copy-file (-> Path-String Path-String Any Void)])

(require "typeset/renderer.rkt")
(require "typeset/tex.rkt")
(require "exec.rkt")

(require "system.rkt")

; register renders
(require "typeset/bin/latex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-exec : (-> Symbol Path-String Path-String [#:fallback Symbol] Void)
  (lambda [renderer src.tex dest-dir #:fallback [fallback 'latex]]
    (define TEXNAME : (Option Path) (file-name-from-path src.tex))
    (define latex : (Option Tex-Renderer)
      (hash-ref latex-database renderer
                (λ [] (hash-ref latex-database fallback (λ [] #false)))))

    (cond [(not (tex-renderer? latex)) (error 'tex "no such a tex renderer: ~a in ~a" renderer (tex-list-renderers))]
          [(not (path? TEXNAME)) (error 'tex "not an input tex file: ~a" src.tex)]
          [else (let ([src-filter (tex-renderer-filter latex)]
                      [TEXNAME.tex (build-path dest-dir TEXNAME)])
                  (make-directory* dest-dir)
                  (cond [(not src-filter) (unless (equal? src.tex TEXNAME.tex) (copy-file src.tex TEXNAME.tex #true))])
                  (parameterize ([current-directory dest-dir])
                    (fg-exec 'tex (tex-renderer-program latex)
                             (list (list "-interaction=batchmode") (list (path->string TEXNAME.tex)))
                             digimon-system)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-list-renderers : (-> (Listof Symbol))
  (lambda []
    (hash-keys latex-database)))
