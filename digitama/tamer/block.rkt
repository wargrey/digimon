#lang racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/format)
(require racket/function)

(require scribble/manual)
(require scribble/core)
(require scribble/decode)
(require scribble/html-properties)
(require scribble/latex-properties)
(require setup/main-collects)

(require "../tamer.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-tamer-indexed-block stx)
  (syntax-parse stx #:datum-literals []
    [(_ id (~optional (~seq #:anchor anchor) #:defaults ([anchor #'#true]))
        [args ...] #:with [pre-flows] #:λ make-block ...)
     (with-syntax* ([Id (datum->syntax #'id (string->symbol (string-titlecase (symbol->string (syntax->datum #'id)))))]
                    [id:tyle (format-id #'id "digimon:tamer:~a" (syntax->datum #'id))]
                    [tamer-id (format-id #'id "tamer-~a" (syntax->datum #'id))]
                    [tamer-id* (format-id #'id "tamer-~a*" (syntax->datum #'id))]
                    [tamer-id** (format-id #'id "tamer-~a**" (syntax->datum #'id))]
                    [tamer-id-here (format-id #'id "tamer-~a-here" (syntax->datum #'id))]
                    [tamer-id-ref (format-id #'id "tamer-~a-ref" (syntax->datum #'id))]
                    [Tamer-Id-ref (format-id #'id "Tamer-~a-ref" (syntax->datum #'Id))]
                    [tamer-default-id-label (format-id #'id "tamer-default-~a-label" (syntax->datum #'id))]
                    [tamer-default-id-label-separator (format-id #'id "tamer-default-~a-label-separator" (syntax->datum #'id))]
                    [tamer-default-id-label-style (format-id #'id "tamer-default-~a-label-style" (syntax->datum #'id))]
                    [tamer-default-id-caption-style (format-id #'id "tamer-default-~a-caption-style" (syntax->datum #'id))])
       #'(begin (define tamer-default-id-label (make-parameter (symbol->string 'Id)))
                (define tamer-default-id-label-separator (make-parameter ": "))
                (define tamer-default-id-label-style (make-parameter 'tt))
                (define tamer-default-id-caption-style (make-parameter #false))
                
                (define tamer-id
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-default-id-label) (tamer-default-id-label-separator) caption
                                         figure-style (tamer-default-id-label-style) (tamer-default-id-caption-style)
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id*
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-default-id-label) (tamer-default-id-label-separator) caption
                                         figuremulti-style (tamer-default-id-label-style) (tamer-default-id-caption-style)
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id**
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-default-id-label) (tamer-default-id-label-separator) caption
                                         figuremultiwide-style (tamer-default-id-label-style) (tamer-default-id-caption-style)
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id-here
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-default-id-label) (tamer-default-id-label-separator) caption
                                         herefigure-style (tamer-default-id-label-style) (tamer-default-id-caption-style)
                                         (λ [] make-block ...) 'anchor)))
                
                (define tamer-id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:tyle id ref-element
                                             (string-downcase (tamer-default-id-label)))))
                
                (define Tamer-Id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:tyle id ref-element
                                             (string-titlecase (tamer-default-id-label)))))))]))

(define tamer-indexed-block
  (lambda [id type label sep caption style label-style caption-style make-block anchor]
    (define sym:tag (tamer-block-id->tag id))
    
    (make-tamer-indexed-traverse-block
     #:latex-anchor anchor
     (λ [type chapter-index current-index]
       (define legend
         (make-block-legend sym:tag label sep caption
                            chapter-index current-index
                            label-style caption-style))
       (values sym:tag (list (make-block) legend)))
     type style)))

(define tamer-indexed-block-ref
  (lambda [index-type id ref-element label]
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (if (not maybe-index)
           (racketerror (ref-element (~a label #\space chapter-index #\. '?)))
           (elemref (~a id) (ref-element (~a label #\space chapter-index #\. maybe-index)))))
     index-type
     (tamer-block-id->tag id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-tamer-indexed-traverse-block
  (lambda [traverse index-type [block-style #false] #:latex-anchor [anchor? #true]]
    (define this-index-story (tamer-index-story))

    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-index-story this-index-story])
         (define order (car this-index-story))
         (define this-story (cdr this-index-story))
         (define global-tags (traverse-indexed-tagbase get index-type))
         (define current-index (hash-ref global-tags this-story (λ [] 1)))
         (define-values (current-tag block) (traverse index-type order current-index))

         (define maybe-anchor
           (and anchor?
                (memq 'latex (get 'scribble:current-render-mode null))
                (cond [(eq? anchor? #true) phantomsection]
                      [else (make-paragraph refstepcounter-style
                                            (list (~a anchor?)))])))
         
         (hash-set! global-tags current-tag (cons order current-index))
         (hash-set! global-tags this-story (add1 current-index))
         (traverse-indexed-tagbase set! index-type global-tags get)

         (make-nested-flow block-style
                           (cond [(list? block) (if (not maybe-anchor) block (cons maybe-anchor block))]
                                 [else (if (not maybe-anchor) (list block) (list maybe-anchor block))])))))))

(define make-tamer-indexed-block-ref
  (lambda [resolve index-type tag]
    (define this-index-story (tamer-index-story))
    (define sym:tag
      (cond [(symbol? tag) tag]
            [(string? tag) (string->symbol tag)]
            [else (string->symbol (~a tag))]))
    
    (make-delayed-element
     (λ [render% pthis infobase]
       (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
       (define global-tags (traverse-indexed-tagbase get index-type))
       (define target-info (hash-ref global-tags sym:tag (λ [] (cons (car this-index-story) #false))))
       (resolve index-type (car target-info) (cdr target-info)))
     (λ [] (content-width (resolve index-type (car this-index-story) #false)))
     (λ [] (content->string (resolve index-type (car this-index-story) #false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-block-legend
  (lambda [tag label sep caption chpt-idx self-idx label-style caption-style]
    (make-paragraph centertext-style
                    (list (make-element legend-style
                                        (list (elemtag (symbol->immutable-string tag)
                                                       (make-element label-style
                                                                     (format "~a ~a.~a~a"
                                                                       label chpt-idx self-idx sep)))
                                              (make-element caption-style caption)))))))

(define make-figure-block
  (lambda [content-style content]
    (make-nested-flow content-style (list (make-nested-flow figureinside-style (decode-flow content))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list 'never-indents
          (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))
(define figuremulti-style (make-style "FigureMulti" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define legend-style (make-style "Legend" figure-style-extras))
(define centertext-style (make-style "Centertext" figure-style-extras))

(define phantomsection (make-paragraph (make-style "phantomsection" null) null))
(define refstepcounter-style (make-style "refstepcounter" null))

(define tamer-block-id->tag
  (lambda [id]
    (cond [(symbol? id) id]
          [(string? id) (string->symbol id)]
          [else (string->symbol (~a id))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API layer
(define tamer-center-block-style (make-style "Centerfigure" figure-style-extras))
(define tamer-left-block-style (make-style "Leftfigure" figure-style-extras))
(define tamer-right-block-style (make-style "Rightfigure" figure-style-extras))
