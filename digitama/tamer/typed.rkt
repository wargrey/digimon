#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(define-type Tag-Body (U String Generated-Tag (Pairof Any (Listof Any))))
(define-type Link-Tag (List Symbol Tag-Body))
(define-type Scribble-Message (-> Symbol String Any * Void))

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
 [#:opaque Block block?]
 [#:opaque Content content?]

 [#:struct generated-tag ()
  #:extra-constructor-name make-generated-tag #:type-name Generated-Tag]

 [#:struct style
  ([name : (U String Symbol False)]
   [properties : (Listof Any)])
  #:extra-constructor-name make-style
  #:type-name Style]
 
 [#:struct part
  ([tag-prefix : (U False String HashTableTop)]
   [tags : (Listof Link-Tag)]
   [title-content : (Option (Listof Content))]
   [style : Style]
   [to-collect : (Listof Any)]
   [blocks : (Listof Block)]
   [parts : (Listof Part)])
  #:extra-constructor-name make-part
  #:type-name Part])

(unsafe-require/typed/provide
 scribble/core
 [content->string (-> (Listof Content) String)])

(unsafe-require/typed/provide
 (submod "." unsafe)
 [handbook-tex-render (-> Path Part Path Path (-> Symbol String Any * Any) Void)])
