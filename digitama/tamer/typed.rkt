#lang typed/racket/base

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/path)

  (require scribble/core)
  (require scribble/render)
  (require (prefix-in tex: scribble/latex-render))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-tex-render
    (lambda [handbook.scrbl self TEXNAME.tex hook.rktl dtrace]
      (define dest-dir (path-only TEXNAME.tex))
      (define tags (part-tags self))

      (if (or (null? tags) (equal? (cadar tags) "tamer-book"))
          (dtrace 'info "(handbook-tex-render ~a #:dest ~a #:dest-name ~a)"
                  handbook.scrbl dest-dir (file-name-from-path TEXNAME.tex))
          (dtrace 'info "(handbook-tex-render ~a #:dest ~a #:dest-name ~a #:tag ~a)"
                  handbook.scrbl dest-dir (file-name-from-path TEXNAME.tex)
                  (cadar tags)))
    
      (when (file-exists? hook.rktl)
        (define ecc (dynamic-require hook.rktl 'extra-character-conversions (λ [] #false)))
        (when (procedure? ecc)
          (dtrace 'info "(load-extra-character-conversions ~a)" hook.rktl)
          (tex:extra-character-conversions ecc)))
      
      (render #:render-mixin tex:render-mixin #:dest-dir dest-dir
              (list self) (list TEXNAME.tex))
      (void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 scribble/core
 [#:opaque Part part?])

(unsafe-require/typed/provide
 (submod "." unsafe)
 [handbook-tex-render (-> Path Part Path Path (-> Symbol String Any * Any) Void)])
