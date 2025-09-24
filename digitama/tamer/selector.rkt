#lang typed/racket/base

(require typed/racket/unsafe)

(require "typed.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module typed typed/racket/base
  (provide (all-defined-out))
  
  (require racket/list)

  (require "../minimal/regexp.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-type Handbook-Selector-Datum (U Symbol Integer Regexp Byte-Regexp (Boxof (U Byte-Regexp Regexp))))
  (define-type Handbook-Chapter-Index (U Char Integer))

  (struct handbook-selector
    ([all? : Boolean]
     [preface? : Boolean]
     [intra-part? : Boolean]
     [bonus? : Boolean]
     [seq:includes : (Listof Natural)]
     [re:includes : (Listof (U Regexp Byte-Regexp))]
     [seq:excludes : (Listof Natural)]
     [re:excludes : (Listof (U Regexp Byte-Regexp))])
    #:type-name Handbook-Selector
    #:transparent)
  
  (define make-handbook-selector : (-> (Listof Handbook-Selector-Datum) Handbook-Selector)
    (lambda [opts]
      (let*-values ([(seq:includes rest) (partition exact-nonnegative-integer? opts)]
                    [(seq:excludes rest) (partition exact-integer? rest)]
                    [(reg:includes rest) (partition bs-regexp? rest)]
                    [(reg:excludes rest) (partition box? rest)])
        (handbook-selector (and (memq 'all rest) #true)
                           (and (memq 'preface rest) #true)
                           (and (or (memq 'flatten rest)
                                    (memq 'intra-part rest))
                                #true)
                           (and (or (memq 'auto-section rest)
                                    (memq 'bonus rest))
                                #true)
                           seq:includes reg:includes
                           (map abs seq:excludes)
                           (filter bs-regexp? (map unbox reg:excludes))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-selector-filter : (-> Any (Option (U Handbook-Selector-Datum (Listof Integer))))
    (lambda [opt]
      (cond [(symbol? opt) opt]
            [(exact-integer? opt) opt]
            [(bs-regexp? opt) opt]
            [(string? opt) (regexp (regexp-quote opt))]
            [(pair? opt)
             (let-values ([(s e) (values (car opt) (cdr opt))])
               (cond [(and (exact-integer? s) (exact-integer? e))
                      (cond [(and (>= s 0) (>= e 0))
                             (if (<= s e) (range s (+ e 1)) (range e (+ s 1)))]
                            [(and (< s 0) (< e 0))
                             (if (<= s e) (range s (+ e 1)) (range e (+ s 1)))]
                            [else #false])]
                     [(eq? s '-)
                      (cond [(exact-integer? e) (- e)]
                            [(bs-regexp? e) (box e)]
                            [(string? e) (box (regexp (regexp-quote e)))]
                            [else #false])]
                     [(eq? s '+) (handbook-selector-filter e)]
                     [else #false]))]
            [else #false])))
  
  (define handbook-selector-flatten : (-> Any Handbook-Selector)
    (lambda [options]
      (make-handbook-selector
       (remove-duplicates
        (let parse : (Listof Handbook-Selector-Datum) ([opts : (Listof Handbook-Selector-Datum) null]
                                                       [options : (Listof Any) (if (list? options) options (list options))])
          (if (pair? options)
              (let ([opt (car options)])
                (define filtered-opt
                  (cond [(list? opt) (parse null opt)]
                        [else (handbook-selector-filter opt)]))
                
                (cond [(list? filtered-opt) (parse (append opts filtered-opt) (cdr options))]
                      [(and filtered-opt) (parse (append opts (list filtered-opt)) (cdr options))]
                      [else (parse opts (cdr options))]))
              opts))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-selector->procedure : (-> Handbook-Selector (-> String Integer Boolean))
    (lambda [self]
      (λ [tag seq]
        #false)))
  
  (define handbook-chapter-index : (-> (Option Handbook-Chapter-Index) (Listof Symbol) (Listof Symbol) Handbook-Chapter-Index)
    (lambda [idx prps parent]
      (cond [(memq 'unnumbered prps)
             (cond [(not idx)   '#:start    -1]
                   [(char? idx) '#:appendix (integer->char (add1 (char->integer idx)))]
                   [(< idx 0)   '#:preface  (- (add1 (- idx)))]
                   [else        '#:appendix #\A])]
            [(exact-positive-integer? idx) (if (memq 'unnumbered parent) #\A (add1 idx))]
            [(char? idx) '#:maybe-wrong (integer->char (add1 (char->integer idx)))]
            [else '#:body-content 1]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module scribbing racket/base
  (provide (all-defined-out))
  (require (submod ".." typed))

  (require scribble/core)
  (require racket/string)

  (require "meta-name.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-offprint
    (lambda [volume opt]
      (let offprint ([children (part-parts volume)]
                     [part-idx #false]
                     [chpt-idx #false]
                     [sretpahc null]
                     [secaferp null]
                     [sunob null]
                     [parent null])
        (if (pair? children)
            (let* ([self (car children)]
                   [rest (cdr children)]
                   [prps (filter symbol? (style-properties (part-style self)))]
                   [tags (map cadr (part-tags self))])
              (cond [(string-prefix? (car tags) handbook-tag-prefix)
                     (offprint rest part-idx chpt-idx sretpahc secaferp (cons self sunob) parent)]

                    ; flatten parts if requested
                    [(and (memq 'grouper prps) (handbook-selector-intra-part? opt))
                     (offprint (append (part-parts self) rest)
                               (handbook-chapter-index part-idx prps null)
                               chpt-idx sretpahc secaferp sunob prps)]

                    [else ; dealing with real offprint units of a book
                     (let ([idx (handbook-chapter-index chpt-idx prps parent)])
                       (if (and (exact-integer? idx) (< idx 0)) ; preface
                           (offprint rest part-idx idx sretpahc (cons self secaferp) sunob parent)
                           (offprint rest part-idx idx sretpahc secaferp sunob parent)))]))
            
            (let ([preface (if (handbook-selector-preface? opt) (reverse secaferp) null)]
                  [bonus (if (handbook-selector-bonus? opt) (reverse sunob) null)])
              (reverse sretpahc))))
      null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide (all-from-out (submod "." typed)))
(require (submod "." typed))

(unsafe-require/typed/provide
 (submod "." scribbing)
 [handbook-offprint (-> Part Handbook-Selector (Listof (Pairof Part Handbook-Chapter-Index)))])
