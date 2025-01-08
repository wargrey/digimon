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

(require "../tamer.rkt")

(require "style.rkt")
(require "backend.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/symbol))

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-tamer-indexed-block stx)
  (syntax-case stx []
    [(_ id #:anchor anchor #:target-style target-style #:legend-style legend-style
        #:with [legend pre-flows args ...] #:do make-block ... #:for [[tamer-id tamer-style] ...])
     (with-syntax* ([Id (datum->syntax #'id (string->symbol (string-titlecase (symbol->immutable-string (syntax->datum #'id)))))]
                    [id:type (format-id #'id "digimon:tamer:~a" (syntax->datum #'id))]
                    [tamer-id-type (format-id #'id "tamer-~a-type" (syntax->datum #'id))]
                    [tamer-id-ref (format-id #'id "tamer-~a-ref" (syntax->datum #'id))]
                    [Tamer-Id-ref (format-id #'id "Tamer-~a-ref" (syntax->datum #'Id))]
                    [tamer-id-label (format-id #'id "tamer-default-~a-label" (syntax->datum #'id))]
                    [tamer-id-label-separator (format-id #'id "tamer-default-~a-label-separator" (syntax->datum #'id))]
                    [tamer-id-label-tail (format-id #'id "tamer-default-~a-label-tail" (syntax->datum #'id))]
                    [tamer-id-label-style (format-id #'id "tamer-default-~a-label-style" (syntax->datum #'id))]
                    [tamer-id-caption-style (format-id #'id "tamer-default-~a-caption-style" (syntax->datum #'id))])
       (syntax/loc stx
         (begin (define tamer-id-label (make-parameter (symbol->immutable-string 'Id)))
                (define tamer-id-label-separator (make-parameter #false))
                (define tamer-id-label-tail (make-parameter #false))
                (define tamer-id-label-style (make-parameter #false))
                (define tamer-id-caption-style (make-parameter #false))
                (define tamer-id-type 'id:type)
                
                (define tamer-id
                  (lambda [id caption args ... . pre-flows]
                    (tamer-indexed-block id 'id:type (tamer-id-label) (tamer-id-label-separator) (tamer-id-label-tail) caption
                                         tamer-style legend-style (tamer-id-label-style) (tamer-id-caption-style) target-style
                                         (λ [legend] make-block ...) 'anchor)))
                ...

                (define tamer-id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:type id ref-element
                                             (string-downcase (tamer-id-label))
                                             (tamer-id-label-separator))))
                
                (define Tamer-Id-ref
                  (lambda [#:elem [ref-element values] id]
                    (tamer-indexed-block-ref 'id:type id ref-element
                                             (string-titlecase (tamer-id-label))
                                             (tamer-id-label-separator)))))))]))

(define-syntax (define-tamer-indexed-figure stx)
  (syntax-parse stx #:datum-literals []
    [(_ id
        (~alt (~optional (~seq #:anchor anchor) #:defaults ([anchor #'#true]))
              (~optional (~seq #:target-style target-style) #:defaults ([target-style #'#false])))
        ...
        [args ...] #:with [legend pre-flows] #:λ make-block ...)
     (with-syntax* ([tamer-id-raw (format-id #'id "tamer-~a-raw" (syntax->datum #'id))]
                    [tamer-id (format-id #'id "tamer-~a" (syntax->datum #'id))]
                    [tamer-id* (format-id #'id "tamer-~a*" (syntax->datum #'id))]
                    [tamer-id! (format-id #'id "tamer-~a!" (syntax->datum #'id))])
       (syntax/loc stx
         (define-tamer-indexed-block id
           #:anchor anchor #:target-style target-style #:legend-style tamer-jfp-legend-style
           #:with [legend pre-flows args ...] #:do make-block ...
           #:for [[tamer-id figure-style]
                  [tamer-id* figuremultiwide-style]
                  [tamer-id! herefigure-style]])))]))

(define tamer-indexed-block-hide-chapter-index (make-parameter #false))
(define tamer-block-label-separator (make-parameter " "))
(define tamer-block-label-tail (make-parameter ": "))
(define tamer-block-label-style (make-parameter 'tt))
(define tamer-block-caption-style (make-parameter #false))

(define tamer-indexed-block
  (lambda [id type label sep tail caption style legend-style label-style caption-style target-style make-block anchor]
    (define sym:tag (tamer-indexed-block-id->symbol id))

    (make-tamer-indexed-traverse-block
     #:latex-anchor anchor
     (λ [type chapter-index current-index]
       (define legend
         (make-block-legend type sym:tag label sep tail caption
                            chapter-index current-index
                            legend-style label-style caption-style target-style))
       (values sym:tag (make-block legend)))
     type style)))

(define tamer-indexed-block-ref
  (lambda [index-type id ref-element label sep]
    (define sym:tag (tamer-indexed-block-id->symbol id))
    
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (define chpt-idx (if (tamer-indexed-block-hide-chapter-index) #false (tamer-block-chapter-label chapter-index)))

       (if (not maybe-index)
           (racketerror (ref-element (if (not chpt-idx)
                                         (~a label (or sep "") '?)
                                         (~a label (or sep "") chpt-idx #\. '?))))
           (make-link-element #false
                              (list (ref-element (if (not chpt-idx)
                                                     (~a label (or sep "") maybe-index)
                                                     (~a label (or sep "") chpt-idx #\. maybe-index))))
                              (tamer-block-sym:tag->tag index-type sym:tag))))
     index-type sym:tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-tamer-indexed-traverse-block
  (lambda [traverse index-type [block-style #false] #:latex-anchor [anchor? #true]]
    (define index-story (tamer-index-story))
    (define index-appendix (tamer-appendix-index))
    
    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-index-story index-story]
                      [tamer-appendix-index index-appendix])
         (define order (car index-story))
         (define this-story (cdr index-story))
         (define global-tags (traverse-indexed-tagbase get index-type))
         (define current-index (hash-ref global-tags this-story (λ [] 1)))
         (define-values (current-tag block) (traverse index-type order current-index))

         (define maybe-anchor
           (and anchor?
                (handbook-latex-renderer? get)
                (cond [(eq? anchor? #true) phantomsection]
                      [else (make-paragraph refstepcounter-style
                                            (list (~a anchor?)))])))
         
         (hash-set! global-tags current-tag (cons order current-index))
         (hash-set! global-tags this-story (add1 current-index))
         (traverse-indexed-tagbase set! index-type global-tags get)

         (make-nested-flow (or block-style quote-style)
                           (if (list? block)
                               (if (not maybe-anchor) block (cons maybe-anchor block))
                               (if (not maybe-anchor) (list block) (list maybe-anchor block)))))))))

(define make-tamer-indexed-block-ref
  (lambda [resolve index-type tag]
    (define sym:tag
      (cond [(symbol? tag) tag]
            [(string? tag) (string->symbol tag)]
            [else (string->symbol (~a tag))]))

    (define index-story (tamer-index-story))
    (define index-appendix (tamer-appendix-index))
    
    (make-delayed-element
     (λ [render% pthis infobase]
       (parameterize ([tamer-index-story index-story]
                      [tamer-appendix-index index-appendix])
         (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
         (define global-tags (traverse-indexed-tagbase get index-type))
         (define target-info (hash-ref global-tags sym:tag (λ [] (cons (car index-story) #false))))
         (resolve index-type (car target-info) (cdr target-info))))
     (λ [] (content-width (resolve index-type (car index-story) #false)))
     (λ [] (content->string (resolve index-type (car index-story) #false))))))

(define tamer-indexed-block-elemtag
  (lambda [id label chapter-index self-index #:separator [sep " "] #:tail [tail ": "] #:type [type #false] #:style [style 'tt]]
    (make-block-label (tamer-indexed-block-id->symbol (or type (string-downcase (~a label))))
                      (tamer-indexed-block-id->symbol id)
                      label sep tail
                      chapter-index self-index
                      style #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-block-label
  (lambda [type tag label sep tail chpt-idx0 self-idx label-style target-style]
    (define chpt-idx (if (tamer-indexed-block-hide-chapter-index) #false (tamer-block-chapter-label chpt-idx0)))
    (define lbl-sep (or sep (tamer-block-label-separator) " "))
    (define lbl-tail (or tail (tamer-block-label-tail) ""))
    (make-target-element target-style
                         (make-element (or label-style (tamer-block-label-style))
                                       (if (or chpt-idx)
                                           (format "~a~a~a.~a~a" label lbl-sep chpt-idx self-idx lbl-tail)
                                           (format "~a~a~a~a"    label lbl-sep self-idx lbl-tail)))
                         (tamer-block-sym:tag->tag type tag))))

(define make-block-legend
  (lambda [type tag label sep tail caption chpt-idx self-idx legend-style label-style caption-style target-style]
    (make-element (or legend-style placeholder-style)
                  (list (make-block-label type tag label sep tail
                                          chpt-idx self-idx label-style target-style)
                        (make-element (or caption-style (tamer-block-caption-style)) caption)))))

(define make-figure-block
  (lambda [legend content-style content [legend-style centeringtext-style]]
    (list (make-nested-flow content-style (list (make-nested-flow figureinside-style (decode-flow content))))
          (make-paragraph legend-style (list legend)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define figure-style-extras
  (list 'never-indents
        (make-css-addition (tamer-stone-source "figure.css"))
        (make-tex-addition (tamer-stone-source "figure.tex"))))

(define figure-target-style
  (make-style #f
              (list
               (make-attributes '((x-target-lift . "Figure")))
               (make-js-addition (tamer-stone-source "figure.js")))))

(define figure-style (make-style "Figure" figure-style-extras))
(define marginfigure-style (make-style "marginfigure" figure-style-extras))
(define herefigure-style (make-style "Herefigure" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define centertext-style (make-style "Centertext" figure-style-extras))
(define centeringtext-style (make-style "Centeringtext" figure-style-extras))
(define margin-legend-style (make-style "MarginLegend" figure-style-extras))

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
          [(< chpt-idx apdx-idx) 0 #| We are in the Preface |#]
          [else (integer->char (+ (- chpt-idx apdx-idx) 65))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API layer
(define tamer-center-block-style (make-style "Centerfigure" figure-style-extras))
(define tamer-left-block-style (make-style "Leftfigure" figure-style-extras))
(define tamer-right-block-style (make-style "Rightfigure" figure-style-extras))

(define tamer-jfp-legend-style (make-style "JFPLegend" figure-style-extras))
