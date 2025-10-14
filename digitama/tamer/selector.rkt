#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require "typed.rkt")

(require "../../emoji.rkt")
(require "../minimal/regexp.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Handbook-Selector-Datum (U Symbol Integer Regexp Byte-Regexp (Boxof (U Byte-Regexp Regexp))))
(define-type Handbook-Chapter-Index (U Char Natural))

(struct handbook-selector
  ([all? : Boolean]
   [preface? : Boolean]
   [intra-part? : Boolean]
   [bonus? : Boolean]
   [seq:includes : (Listof Handbook-Chapter-Index)]
   [tag:includes : (Listof (U Regexp Byte-Regexp))]
   [seq:excludes : (Listof Handbook-Chapter-Index)]
   [tag:excludes : (Listof (U Regexp Byte-Regexp))])
  #:type-name Handbook-Selector
  #:transparent)

(define current-user-specified-selector : (Parameterof (Option Handbook-Selector)) (make-parameter #false))
(define current-user-request-no-volume? : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define make-user-specified-selector : (-> (Listof Handbook-Chapter-Index) Boolean Boolean Handbook-Selector)
  (lambda [seqs flatten? strip?]
    (handbook-selector #false (not strip?) flatten? (not strip?)
                       seqs null null null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-selector->predicate : (-> Handbook-Selector (-> (Listof Any) Handbook-Chapter-Index Boolean))
  (lambda [self]
    (define ok-seqs (handbook-selector-seq:includes self))
    (define ok-tags (handbook-selector-tag:includes self))
    (define ex-seqs (handbook-selector-seq:excludes self))
    (define ex-tags (handbook-selector-tag:excludes self))
    
    (define seq-included? : (-> Handbook-Chapter-Index Any)
      (cond [(handbook-selector-all? self) (λ [seq] #true)]
            [(null? ok-seqs) (λ [seq] #false)]
            [else (λ [seq] (or (memq seq ok-seqs)))]))
    
    (define seq-excluded? : (-> Handbook-Chapter-Index Any)
      (if (null? ex-seqs)
          (λ [seq] #false)
          (λ [seq] (memq seq ex-seqs))))
    
    (define tag-included? : (-> String Any)
      (cond [(null? ok-tags) (λ [tag] #false)]
            [else (λ [tag] (for/or : Boolean ([re (in-list ok-tags)])
                             (regexp-match? re tag)))]))
    
    (define tag-excluded? : (-> String Boolean)
      (if (null? ex-tags)
          (λ [tag] #false)
          (λ [tag] (for/or : Boolean ([re (in-list ex-tags)])
                     (regexp-match? re tag)))))
    
    (λ [tags seq]
      (or (and (seq-included? seq)
               (not (seq-excluded? seq)))
          (for/or ([tag (in-list tags)])
            (and (non-empty-string? tags)
                 (tag-included? tags)
                 (not (tag-excluded? tags))))))))
  
(define handbook-chapter-index : (-> Handbook-Chapter-Index (Listof Symbol) (Option Part) (Option Handbook-Chapter-Index))
  (lambda [idx prps milestone-part]
    (cond [(memq 'unnumbered prps) '#:intact #false]
          [(exact-nonnegative-integer? idx) '#:main-matter (if (not milestone-part) (add1 idx) #\A)]
          [(char? idx) '#:appendix (integer->char (add1 (char->integer idx)))]
          [else '#:main-matter-start 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-offprint : (-> Part Handbook-Selector Scribble-Message
                                (Values (U (Listof Part) (Boxof (Listof Block)))
                                        (Listof (Pairof Part Handbook-Chapter-Index))
                                        (Listof Part)
                                        (Listof Part)))
  (lambda [volume op.cfg dtrace]
    (define chapter-match? (handbook-selector->predicate op.cfg))
    
    (let offprint ([children : (Listof Part) (part-parts volume)]
                   [part-idx : Handbook-Chapter-Index 0]
                   [chpt-idx : Handbook-Chapter-Index 0]
                   [sretpahc : (Listof (Pairof Part Handbook-Chapter-Index)) null]
                   [secaferp : (Listof Part) null]
                   [sunob : (Listof Part) null]
                   [post-skooh : (Listof Part) null]
                   [appendix-part : (Option Part) #false])
      (if (pair? children)
          (let* ([self (car children)]
                 [rest (cdr children)]
                 [prps (filter symbol? (style-properties (part-style self)))]
                 [tags ((inst map Tag-Body Link-Tag) cadr (part-tags self))]
                 [title (content->string (or (part-title-content self) null))])
            (cond [(memq 'bonus prps)
                   (dtrace 'debug "BONUS~a: ~a ~a" prps title tags)
                   (offprint rest part-idx chpt-idx sretpahc secaferp (cons self sunob) post-skooh appendix-part)]

                  [(memq 'post-hook prps)
                   (dtrace 'debug "HOOK~a: ~a" prps tags)
                   (offprint rest part-idx chpt-idx sretpahc secaferp sunob (cons self post-skooh) appendix-part)]
                  
                  ; flatten parts if requested
                  [(and (memq 'grouper prps) (handbook-selector-intra-part? op.cfg))
                   (if (memq 'fin prps)
                       (begin ; begin back body
                         (dtrace 'debug "BACK~a: ~a ~a" prps title tags)
                         (offprint rest part-idx chpt-idx sretpahc secaferp sunob post-skooh (or appendix-part self)))
                       (let ([idx (handbook-chapter-index part-idx prps appendix-part)])
                         (dtrace 'debug "PART ~a~a: ~a ~a" idx prps title tags)
                         (offprint (append (part-parts self) rest) (or idx part-idx) chpt-idx sretpahc secaferp sunob post-skooh appendix-part)))]
                  
                  [else ; dealing with real offprint units of a book
                   (let ([idx (handbook-chapter-index chpt-idx prps appendix-part)])
                     (cond [(and idx (chapter-match? tags idx))
                            (dtrace 'debug "~a: ~a ~a ~a" idx title tags pin#)
                            (offprint rest part-idx idx (cons (cons self idx) sretpahc) secaferp sunob post-skooh appendix-part)]
                           
                           [(not idx) ; TODO: deal with other unnumbered sections
                            (if (or (memq 'fin prps) (char? chpt-idx) (> chpt-idx 0))
                                (begin ; begin back body
                                  (dtrace 'debug "BACK~a: ~a ~a" prps title tags)
                                  (offprint rest part-idx chpt-idx sretpahc secaferp sunob post-skooh (or appendix-part self)))
                                (begin ; front body
                                  (dtrace 'debug "FRONT~a: ~a ~a" prps title tags)
                                  (offprint rest part-idx chpt-idx sretpahc (cons self secaferp) sunob post-skooh appendix-part)))]
                           
                           [else
                            (dtrace 'debug "~a: ~a ~a" idx title tags)
                            (offprint rest part-idx (or idx chpt-idx) sretpahc secaferp sunob post-skooh appendix-part)]))]))
          
          (values (cond [(handbook-selector-preface? op.cfg) (reverse secaferp)]
                        [(null? secaferp) null]
                        [else (let search-latex-hooks ([skcolb : (Listof Block) (reverse (part-blocks (car secaferp)))]
                                                       [hook-blocks : (Listof Block) null])
                                (if (pair? skcolb)
                                    (let-values ([(self rest) (values (car skcolb) (cdr skcolb))])
                                      (cond [(and (compound-paragraph? self)
                                                  (memq 'pre-hook (style-properties (compound-paragraph-style self))))
                                             (search-latex-hooks rest (cons self hook-blocks))]

                                            ; stop when we see the table of content
                                            [(and (delayed-block? self)
                                                  (eq? (object-name (delayed-block-resolve self))
                                                       'handbook-smart-table))
                                             (search-latex-hooks null (cons self hook-blocks))]
                                            
                                            [else (search-latex-hooks rest hook-blocks)]))
                                    (box hook-blocks)))])
                  (reverse sretpahc)
                  (if (and appendix-part)
                      (append (reverse post-skooh) (list appendix-part))
                      (reverse post-skooh))
                  (if (handbook-selector-bonus? op.cfg) (reverse sunob) null))))))
