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
    [(_ id (~alt (~optional (~seq #:anchor anchor) #:defaults ([anchor #'#true]))
                 (~optional (~seq #:target-style target-style) #:defaults ([target-style #'#false])))
        ...
        [args ...] #:with [pre-flows] #:λ make-block ...)
     (with-syntax* ([Id (datum->syntax #'id (string->symbol (string-titlecase (symbol->string (syntax->datum #'id)))))]
                    [id:tyle (format-id #'id "digimon:tamer:~a" (syntax->datum #'id))]
                    [tamer-id (format-id #'id "tamer-~a" (syntax->datum #'id))]
                    [tamer-id* (format-id #'id "tamer-~a*" (syntax->datum #'id))]
                    [tamer-id** (format-id #'id "tamer-~a**" (syntax->datum #'id))]
                    [tamer-id-here (format-id #'id "tamer-~a-here" (syntax->datum #'id))]
                    [tamer-id-ref (format-id #'id "tamer-~a-ref" (syntax->datum #'id))]
                    [Tamer-Id-ref (format-id #'id "Tamer-~a-ref" (syntax->datum #'Id))]
                    [tamer-id-label (format-id #'id "tamer-default-~a-label" (syntax->datum #'id))]
                    [tamer-id-label-separator (format-id #'id "tamer-default-~a-label-separator" (syntax->datum #'id))]
                    [tamer-id-label-style (format-id #'id "tamer-default-~a-label-style" (syntax->datum #'id))]
                    [tamer-id-caption-style (format-id #'id "tamer-default-~a-caption-style" (syntax->datum #'id))])
       #'(begin (define tamer-id-label (make-parameter (symbol->string 'Id)))
                (define tamer-id-label-separator (make-parameter ": "))
                (define tamer-id-label-style (make-parameter 'tt))
                (define tamer-id-caption-style (make-parameter #false))
                
                (define tamer-id
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-id-label) (tamer-id-label-separator) caption
                                         figure-style (tamer-id-label-style) (tamer-id-caption-style) target-style
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id*
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-id-label) (tamer-id-label-separator) caption
                                         figuremulti-style (tamer-id-label-style) (tamer-id-caption-style) target-style
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id**
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-id-label) (tamer-id-label-separator) caption
                                         figuremultiwide-style (tamer-id-label-style) (tamer-id-caption-style) target-style
                                         (λ [] make-block ...) 'anchor)))

                (define tamer-id-here
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:tyle (tamer-id-label) (tamer-id-label-separator) caption
                                         herefigure-style (tamer-id-label-style) (tamer-id-caption-style) target-style
                                         (λ [] make-block ...) 'anchor)))
                
                (define tamer-id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:tyle id ref-element
                                             (string-downcase (tamer-id-label)))))
                
                (define Tamer-Id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:tyle id ref-element
                                             (string-titlecase (tamer-id-label)))))))]))

(define tamer-indexed-block
  (lambda [id type label sep caption style label-style caption-style target-style make-block anchor]
    (define sym:tag (tamer-indexed-block-id->symbol id))
    
    (make-tamer-indexed-traverse-block
     #:latex-anchor anchor
     (λ [type chapter-index current-index]
       (define legend
         (make-block-legend type sym:tag label sep caption
                            chapter-index current-index
                            label-style caption-style target-style))
       (values sym:tag (list (make-block) legend)))
     type style)))

(define tamer-indexed-block-ref
  (lambda [index-type id ref-element label]
    (define sym:tag (tamer-indexed-block-id->symbol id))
    
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (define chpt-idx (tamer-block-chapter-label chapter-index))
       (if (not maybe-index)
           (racketerror (ref-element (~a label #\space chpt-idx #\. '?)))
           (make-link-element #false
                              (list (ref-element (~a label #\space chpt-idx #\. maybe-index)))
                              (tamer-block-sym:tag->tag index-type sym:tag))))
     index-type sym:tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-tamer-indexed-traverse-block
  (lambda [traverse index-type [block-style #false] #:latex-anchor [anchor? #true]]
    (define this-index-story (tamer-index-story))
    (define appendix-index (tamer-appendix-index))

    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-index-story this-index-story]
                      [tamer-appendix-index appendix-index])
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
    (define appendix-index (tamer-appendix-index))
    
    (define sym:tag
      (cond [(symbol? tag) tag]
            [(string? tag) (string->symbol tag)]
            [else (string->symbol (~a tag))]))
    
    (make-delayed-element
     (λ [render% pthis infobase]
       (parameterize ([tamer-index-story this-index-story]
                      [tamer-appendix-index appendix-index])
         (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
         (define global-tags (traverse-indexed-tagbase get index-type))
         (define target-info (hash-ref global-tags sym:tag (λ [] (cons (car this-index-story) #false))))
         (resolve index-type (car target-info) (cdr target-info))))
     (λ [] (content-width (resolve index-type (car this-index-story) #false)))
     (λ [] (content->string (resolve index-type (car this-index-story) #false))))))

(define tamer-indexed-block-elemtag
  (lambda [id label chapter-index self-index #:separator [sep ": "] #:type [type #false] #:style [style 'tt]]
    (make-block-label (tamer-indexed-block-id->symbol (or type (string-downcase (~a label))))
                      (tamer-indexed-block-id->symbol id)
                      label sep
                      chapter-index self-index
                      style #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-block-label
  (lambda [type tag label sep chpt-idx0 self-idx label-style target-style]
    (define chpt-idx (tamer-block-chapter-label chpt-idx0))
    (make-target-element target-style
                         (make-element label-style
                                       (cond [(not sep) (format "~a ~a.~a" label chpt-idx self-idx)]
                                             [else (format "~a ~a.~a~a" label chpt-idx self-idx sep)]))
                         (tamer-block-sym:tag->tag type tag))))

(define make-block-legend
  (lambda [type tag label sep caption chpt-idx self-idx label-style caption-style target-style]
    (make-paragraph centertext-style
                    (list (make-element legend-style
                                        (list (make-block-label type tag label sep
                                                                chpt-idx self-idx label-style target-style)
                                              (make-element caption-style caption)))))))

(define make-figure-block
  (lambda [content-style content]
    (make-nested-flow content-style (list (make-nested-flow figureinside-style (decode-flow content))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-block-source
  (lambda [.res]
    (collection-file-path .res "digimon" "stone")))

(define figure-style-extras
  (list 'never-indents
        (make-css-addition (tamer-block-source "figure.css"))
        (make-tex-addition (tamer-block-source "figure.tex"))))

(define figure-target-style
  (make-style #f
              (list
               (make-attributes '((x-target-lift . "Figure")))
               (make-js-addition (tamer-block-source "figure.js")))))

(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))
(define figuremulti-style (make-style "FigureMulti" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define legend-style (make-style "Legend" figure-style-extras))
(define centertext-style (make-style "Centertext" figure-style-extras))

(define phantomsection (make-paragraph (make-style "phantomsection" null) null))
(define refstepcounter-style (make-style "refstepcounter" null))

(define tamer-indexed-block-id->symbol
  (lambda [id]
    (cond [(symbol? id) id]
          [(string? id) (string->symbol id)]
          [else (string->symbol (~a id))])))

(define tamer-block-sym:tag->tag
  (lambda [type tag]
    (list type (symbol->immutable-string tag))))

(define tamer-block-chapter-label
  (lambda [chpt-idx]
    (define apdx-idx (tamer-appendix-index))

    (cond [(not apdx-idx) chpt-idx]
          [(< chpt-idx apdx-idx) 0 #| It's in the preface |#]
          [else (integer->char (+ (- chpt-idx apdx-idx) 65))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API layer
(define tamer-center-block-style (make-style "Centerfigure" figure-style-extras))
(define tamer-left-block-style (make-style "Leftfigure" figure-style-extras))
(define tamer-right-block-style (make-style "Rightfigure" figure-style-extras))
