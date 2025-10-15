#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../scribble.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/path)
  (require racket/list)

  (require scribble/core)
  (require scribble/render)

  (require setup/xref)
  
  (require (prefix-in tex: scribble/latex-render))
  (require (prefix-in html: scribble/html-render))
  (require (prefix-in mrkd: scribble/markdown-render))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-tex-render
    (lambda [handbook.scrbl self.doc TEXNAME.tex hook.rktl dtrace]
      (define dest-dir (path-only TEXNAME.tex))
      
      (dtrace 'info "(handbook-tex-render ~a #:dest ~a #:dest-name ~a #:tag ~a)"
              handbook.scrbl dest-dir (file-name-from-path TEXNAME.tex)
              (cadar (part-tags self.doc)))
    
      (when (file-exists? hook.rktl)
        (define ecc (dynamic-require hook.rktl 'extra-character-conversions (λ [] #false)))
        (when (procedure? ecc)
          (dtrace 'info "(load-extra-character-conversions ~a)" hook.rktl)
          (tex:extra-character-conversions ecc)))
      
      (render #:render-mixin tex:render-mixin #:dest-dir dest-dir
              (list self.doc) (list TEXNAME.tex))))

  (define handbook-html-render
    (lambda [handbook.scrbl dest-dir dtrace]
      (define self.doc (dynamic-require handbook.scrbl 'doc))

      (dtrace 'info "(handbook-multi-html-render ~a #:dest ~a #:tag ~a)"
              handbook.scrbl dest-dir (map cadr (part-tags self.doc)))
      
      (render (list self.doc) (list handbook.scrbl)
              #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %))) #:dest-dir dest-dir
              #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))
              #:image-preferences '(svg png gif pdf))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define dist-extract-readme
    (lambda [self.doc start end0]
      (let* ([subparts (part-parts self.doc)]
             [size (length subparts)]
             [endp1 (if (not end0) size (min end0 size))]
             [span (- endp1 start)])
        (cond [(or (<= span 0) (>= start size)) (values (struct-copy part self.doc [parts null]) 0 0)]
              [(= start 0) (values (struct-copy part self.doc [parts (take subparts span)]) start endp1)]
              [else (values (struct-copy part self.doc [parts (take (list-tail subparts start) span)]) start endp1)]))))

  (define handbook-readme-render
    (lambda [handbook.scrbl handbook.md start0 end0 dtrace]
      (define-values (self.doc start endp1) (dist-extract-readme (dynamic-require handbook.scrbl 'doc) start0 end0))
      (define dest-dir (path-only handbook.md))
      
      (dtrace 'info "(handbook-readme-render ~a #:dest ~a #:dest-name ~a #:tag ~a #:range ~a)"
              handbook.scrbl dest-dir (file-name-from-path handbook.md)
              (cadar (part-tags self.doc)) (and (> endp1 start) (cons start endp1)))

      (render #:dest-dir dest-dir #:render-mixin mrkd:render-mixin
              (list self.doc) (list (file-name-from-path handbook.md)))))
  
  (define handbook-mkd-render
    (lambda [handbook.scrbl self.doc handbook.md dtrace]
      (define dest-dir (path-only handbook.md))
      
      (dtrace 'info "(handbook-markdown-render ~a #:dest ~a #:dest-name ~a #:tag ~a)"
              handbook.scrbl dest-dir (file-name-from-path handbook.md)
              (cadar (part-tags self.doc)))

      (render #:dest-dir dest-dir #:render-mixin mrkd:render-mixin
              (list self.doc) (list (file-name-from-path handbook.md))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [handbook-tex-render (-> Path Part Path Path Scribble-Message Void)]
 [handbook-html-render (-> Path Path Scribble-Message Void)]
 [handbook-mkd-render (-> Path Part Path Scribble-Message Void)]
 [handbook-readme-render (-> Path Path Index (Option Index) Scribble-Message Void)])