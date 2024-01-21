#lang racket/base

(provide (all-defined-out))

(require racket/list)
(require file/convertible)

(require scribble/core)
(require scribble/html-properties)
(require scribble/latex-properties)

(require "shadow.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-metainfo
  (lambda [pthis]
    (define maybe-authors
      (let search-authors ([blocks (part-blocks pthis)])
        (and (pair? blocks)
             (or (let ([block (car blocks)])
                   (cond [(paragraph? block) (and (eq? (style-name (paragraph-style block)) 'author) (paragraph-content block))]
                         [(compound-paragraph? block) (search-authors (compound-paragraph-blocks block))]
                         [else #false]))
                 (search-authors (cdr blocks))))))
    
    (values (content->string (or (part-title-content pthis) null))
            (if (list? maybe-authors) (map content->string maybe-authors) null))))

(define handbook-dependencies
  (lambda [pthis render-mode]
    (remove-duplicates
     (handbook-part-extract-path pthis render-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-part-extract-path
  (lambda [pthis render-mode]
    (append (handbook-style-extract-path (part-style pthis) render-mode)

            (apply append
                   (append (for/list ([block (in-list (part-blocks pthis))])
                             (handbook-block-extract-path block render-mode))
                   
                           (for/list ([subpart (in-list (part-parts pthis))])
                             (handbook-part-extract-path subpart render-mode)))))))

(define handbook-block-extract-path
  (lambda [bthis render-mode]
    (cond [(paragraph? bthis) (handbook-block-content-extract-path (paragraph-style bthis) (paragraph-content bthis) render-mode)]
          [(nested-flow? bthis) (handbook-compound-block-extract-path (nested-flow-style bthis) (nested-flow-blocks bthis) render-mode)]
          [(itemization? bthis) (handbook-compound-block-extract-path (itemization-style bthis) (flatten (itemization-blockss bthis)) render-mode)]
          [(table? bthis) (handbook-compound-block-extract-path (table-style bthis) (filter block? (flatten (table-blockss bthis))) render-mode)]
          [(compound-paragraph? bthis) (handbook-compound-block-extract-path (compound-paragraph-style bthis) (compound-paragraph-blocks bthis) render-mode)]
          [(traverse-block/shadow? bthis) (let ([s ((traverse-block/shadow-block bthis))]) (if (block? s) (handbook-block-extract-path s render-mode) null))]
          [else null])))

(define handbook-compound-block-extract-path
  (lambda [style children render-mode]
    (apply append
           (cons (handbook-style-extract-path style render-mode)
                 (for/list ([child (in-list children)])
                   (handbook-block-extract-path child render-mode))))))

(define handbook-block-content-extract-path
  (lambda [style children render-mode]
    (append (handbook-style-extract-path style render-mode)

            (apply append
                   (for/list ([child (if (list? children) (in-list children) (in-value children))])
                     (or (and (traverse-element/shadow? child)
                              (let ([e ((traverse-element/shadow-element child))])
                                (and (element? e)
                                     (handbook-element-extract-path e render-mode))))
                         (and (element? child)
                              (handbook-element-extract-path child render-mode))
                         (and (convertible? child)
                              (let ([maybe-path (convert child 'script-path #false)])
                                (cond [(path-literal? maybe-path) (list (path-identity maybe-path))]
                                      [(pair? maybe-path) (for/list ([p (in-list maybe-path)] #:when (path-literal? p)) (path-identity p))]
                                      [else #false])))
                         null))))))
  
(define handbook-element-extract-path
  (lambda [elem render-mode]
    (define-values (style-paths body-path) (handbook-raw-element-extract-path elem render-mode))

    (cond [(and (pair? style-paths) (path-literal? body-path)) (cons (path-identity body-path) style-paths)]
          [(path-literal? body-path) (list (path-identity body-path))]
          [(pair? style-paths) style-paths]
          [else null])))

(define handbook-raw-element-extract-path
  (lambda [elem render-mode]
    (values (let ([s (element-style elem)])
              (and (style? s)
                   (handbook-style-extract-path s render-mode)))
            
            (cond [(image-element? elem) (image-element-path elem)]))))

(define handbook-style-extract-path
  (lambda [style render-mode]
    (let extract ([props (style-properties style)]
                  [shtap null])
      (if (pair? props)
          (let-values ([(self rest) (values (car props) (cdr props))])
            (cond [(tex-addition? self) (extract rest (style-path-cons (tex-addition-path self) shtap 'latex render-mode))]
                  [(css-addition? self) (extract rest (style-path-cons (css-addition-path self) shtap 'html render-mode))]
                  [(js-addition? self) (extract rest (style-path-cons (js-addition-path self) shtap 'html render-mode))]
                  [(css-style-addition? self) (extract rest (style-path-cons (css-style-addition-path self) shtap 'html render-mode))]
                  [(js-style-addition? self) (extract rest (style-path-cons (js-style-addition-path self) shtap 'html render-mode))]
                  [else (extract rest shtap)]))
          (reverse shtap)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define style-path-cons
  (lambda [maybe-path paths target-render-mode user-required-render-mode]
    (or (and (eq? target-render-mode user-required-render-mode)
             (path-literal? maybe-path)
             (file-exists? maybe-path)
             (cons (path-identity maybe-path) paths))
        paths)))
