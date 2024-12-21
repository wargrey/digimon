#lang racket/base

(provide (all-defined-out))

(require "tag.rkt")
(require "misc.rkt")
(require "block.rkt")
(require "theme.rkt")
(require "texbook.rkt")

(require racket/symbol)

(require scribble/core)
(require scribble/manual)
(require scribble/decode)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (algo-pseudocode stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:tag maybe-tag) #:defaults ([maybe-tag #'#false]))) ...
        title lines ...)
     (syntax/loc stx
       (let ([tag (or maybe-tag (symbol->immutable-string (gensym 'algo:)))])
         (parameterize ([current-algorithm tag])
           (algorithm-pseudocode #:tag tag title lines ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define algo-pseudocode-index-type 'algorithm)
(define algo-pseudocode-style (make-style "PlainBox" (list 'command)))

(define algo-column-hspace (hspace 1))
(define algo-title-hspace (hspace 4))

(define tamer-default-algorithm-label (make-parameter "Algorithm"))
(define current-algorithm (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define algo-ref
  (lambda [#:line [line #false] algotag]
    (algo-pseudocode-ref #:elem subscript #:line line #:hide-label? #false algotag)))

(define Algo-Ref
  (lambda [#:line [line #false] algotag]
    (algo-pseudocode-ref #:elem values #:line line #:hide-label? #false algotag)))

(define algoref
  (lambda [#:line [line #false] algotag]
    (algo-pseudocode-ref #:elem subscript #:line line #:hide-label? #true algotag)))

(define AlgoRef
  (lambda [#:line [line #false] algotag]
    (algo-pseudocode-ref #:elem values #:line line #:hide-label? #true algotag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $tex:newcounter:algorithm
  (lambda []
    ($tex:newcounter algo-pseudocode-index-type)))

(define algorithm-pseudocode
  (lambda [#:tag [maybe-tag #false] title . lines]
    (define algo-tag (or maybe-tag (gensym 'algo:)))

    (define pcode-frows
      (for/list ([line (in-list lines)]
                 [lNo. (in-naturals 1)])
        (define line-No. (number->string lNo.))
        (define line-name (handbook-content-filter (car line)))
        (define body (decode-content (cdr line)))
        (define sublines
          (let partition ([senil null]
                          [enil null]
                          [rest body])
            (cond [(pair? rest)
                   (let-values ([(self tail) (values (car rest) (cdr rest))])
                     (cond [(not (handbook-newline-element? self)) (partition senil (cons self enil) tail)]
                           [(null? senil) (partition (list (list (decode-content (reverse enil)))) null tail)]
                           [else (partition (cons (list (decode-content (reverse enil))) senil) null tail)]))]
                  [(pair? senil) (tabular (reverse (cons (list (decode-content (reverse enil))) senil)))]
                  [else body])))
        
        (list (λ [type] (tamer-elemtag #:type type (format "~a#L~a" algo-tag line-No.) (envvar line-No.)))
              (λ [type] (tamer-elemtag #:type type (format "~a#[~a]" algo-tag line-name) (exec "[" line-name "]")))
              sublines)))
  
    (make-tamer-indexed-traverse-block
     #:latex-anchor algo-pseudocode-index-type
     (λ [type chapter-index current-index]
       (values algo-tag
               (tabular (list (list (para ($tex:phantomsection)
                                          (tamer-indexed-block-elemtag #:type type #:tail "" algo-tag
                                                                       (tamer-default-algorithm-label)
                                                                       chapter-index current-index)
                                          algo-title-hspace title))
                              
                              ;;; NOTICE: row borders might also prevent page breaks
                              (list (tabular #:style 'boxed
                                             #:sep algo-column-hspace
                                             #:column-properties '(left right left)
                                             #:row-properties (if (null? pcode-frows) null '(top))
                                             (for/list ([λrows (in-list pcode-frows)])
                                               (list ($tex:phantomsection)
                                                     ((car λrows) type)
                                                     ((cadr λrows) type)
                                                     (caddr λrows)))))))))
     algo-pseudocode-index-type
     algo-pseudocode-style)))

(define algo-pseudocode-ref
  (lambda [#:elem [algo-element subscript] #:line [line #false] #:hide-label? [hide-label? #false] algo-tag]
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (if (pair? line)
           (let-values ([(line0 linen) (values (car line) (cdr line))])
             (if (not maybe-index)
                 (racketerror (algo-element (format "~a~a:~a-~a" (tamer-default-algorithm-label) chapter-index line0 linen)))
                 (algo-element (list (tamer-elemref #:type type (algo-format line0 "~a" algo-tag)
                                                    (if (and hide-label? line0)
                                                        (:mod:link (algo-label line0))
                                                        (:mod:link (algo-format line0
                                                                                "~a~a.~a" (tamer-default-algorithm-label)
                                                                                chapter-index maybe-index))))
                                     (:pn "-")
                                     (tamer-elemref #:type type (algo-format linen "~a" algo-tag)
                                                    (:mod:link (algo-label linen)))))))
           (if (not maybe-index)
               (racketerror (algo-element (algo-format line "~a~a" (tamer-default-algorithm-label) chapter-index)))
               (algo-element (tamer-elemref #:type type (algo-format line "~a" algo-tag)
                                            (if (and hide-label? line)
                                                (:mod:link (algo-label line))
                                                (:mod:link (algo-format line
                                                                        "~a~a.~a" (tamer-default-algorithm-label)
                                                                        chapter-index maybe-index))))))))
     algo-pseudocode-index-type algo-tag)))

(define algo-goto
  (lambda [#:elem [algo-element values] #:tag [tag #false] line]
    (define algo-tag (or tag (current-algorithm)))
    
    (when (not algo-tag)
      (raise-user-error (object-name algo-goto) "should be used inside `algo-pseudocode`"))

    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (algo-element (tamer-elemref #:type type (algo-format line "~a" algo-tag)
                                    (:mod:link (algo-label line)))))
     algo-pseudocode-index-type algo-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define algo-label
  (lambda [line]
    (format (if (exact-integer? line) "L~a" "[~a]") line)))

(define algo-format
  (lambda [line fmt . argv]
    (if (or (not line) (eq? line '||))
        (apply format fmt argv)
        (string-append (apply format fmt argv)
                       "#" (algo-label line)))))
