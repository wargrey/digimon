#lang typed/racket/base

(provide (all-defined-out))

(require "typed.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/class)
  (require file/convertible)

  (require scribble/core)
  (require scribble/base-render)
  (require scribble/html-properties)
  (require scribble/latex-properties)
  
  (require "style.rkt")
  (require "backend.rkt")
  
  (require "../../filesystem.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define handbook-metainfo
    (lambda [pthis]
      (define maybe-authors
        (let search-authors ([blocks (part-blocks pthis)])
          (and (pair? blocks)
               (or (let ([block (car blocks)])
                     (cond [(paragraph? block)
                            (and (eq? (style-name (paragraph-style block)) 'author)
                                 (paragraph-content block))]
                           [(compound-paragraph? block)
                            (search-authors (compound-paragraph-blocks block))]
                           [else #false]))
                   (search-authors (cdr blocks))))))
      
      (values (content->string (or (part-title-content pthis) null))
              (if (list? maybe-authors) (map content->string maybe-authors) null))))
  
  (define handbook-scripts
    (lambda [part type]
      ; the required arguments of `render`
      (define docs (list part))
      (define names (list #false))
      
      (define script (make-object scrbl-script% type))
      
      (define t:metrics (send script traverse docs names))
      (define c:metrics (send script collect  docs names t:metrics))
      (define r:metrics (send script resolve  docs names c:metrics))
      
      (send script render docs names r:metrics)
      
      (hash-keys (get-field scripts script))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define scrbl-script%
    (class render%
      (super-new [dest-dir #false])
      
      (init-field render-type)
      
      (field [scripts (make-hash)])
      
      (inherit render-block)

      (define/override (current-render-mode) (list render-type handbook-stat-render-name))
      (define/override (get-suffix) '#:who-cares #".wc")
      (define/override (install-file fn [content #f] #:private-name? [private-name? #t]) (void))
      (define/override (table-of-contents part ri) empty-block)
      (define/override (local-table-of-contents part ri) empty-block)

      (define/override (render-part self ri)
        (extract-style-path (part-style self))
        (super render-part self ri))

      (define/override (render-paragraph self parent ri)
        (extract-style-path (paragraph-style self))
        (super render-paragraph self parent ri))

      (define/override (render-compound-paragraph self parent ri starting-item?)
        (extract-style-path (compound-paragraph-style self))
        (super render-compound-paragraph self parent ri starting-item?))
    
      (define/override (render-nested-flow self parent ri starting-item?)
        (extract-style-path (nested-flow-style self))
        (super render-nested-flow self parent ri starting-item?))
  
      (define/override (render-itemization self parent ri)
        (extract-style-path (itemization-style self))
        (super render-itemization self parent ri))

      (define/override (render-table self parent ri starting-item?)
        (extract-style-path (table-style self))
        (for/list ([cell (in-list (apply append (table-blockss self)))]
                   #:when (block? cell))
          (render-block cell parent ri starting-item?)))

      (define/override (render-auxiliary-table self parent ri)
        (render-table self parent ri #false))
    
      (define/override (render-content self parent ri)
        (cond [(element? self)
               (extract-style-path* (element-style self))
               (when (image-element? self)
                 (push-path* (image-element-path self)
                             (image-element-suffixes self)))]
              [(multiarg-element? self) (extract-style-path* (multiarg-element-style self))]
              [(convertible? self) (convertible-script-paths self)])
        (super render-content self parent ri))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (extract-style-path* maybe-style)
        (when (style? maybe-style)
          (extract-style-path maybe-style)))
    
      (define (extract-style-path style)
        (for ([prop (in-list (style-properties style))])
          (cond [(tex-addition? prop) (push-style-path (tex-addition-path prop) 'latex render-type)]
                [(css-addition? prop) (push-style-path (css-addition-path prop) 'html render-type)]
                [(js-addition? prop) (push-style-path (js-addition-path prop) 'html render-type)]
                [(css-style-addition? prop) (push-style-path (css-style-addition-path prop) 'html render-type)]
                [(js-style-addition? prop) (push-style-path (js-style-addition-path prop) 'html render-type)]
                [(convertible? prop) (convertible-script-paths prop)]
                [(path-literal? prop) (push-path prop)])))

      (define (push-style-path maybe-path target-render-mode user-required-render-mode)
        (when (and (eq? target-render-mode user-required-render-mode)
                   (path-literal? maybe-path))
          (push-path maybe-path)))

      (define (push-path* path suffixes)
        (when (path-literal? path)
          (cond [(file-exists? path) (push-path path)]
                [else (for ([ext (in-list suffixes)])
                        (define path.ext (path-add-extension path ext))
                        (when (file-exists? path.ext)
                          (push-path path.ext)))])))
    
      (define (push-path path)
        (when (file-exists? path)
          (define self (simple-form-path (path-identity path)))
          (unless (hash-has-key? scripts self)
            (hash-set! scripts self #true))))
    
      (define (convertible-script-paths c)
        (define maybe-path (convert c 'script-path #false))

        (cond [(path-literal? maybe-path)
               (push-path maybe-path)]
              [(list? maybe-path)
               (for ([p (in-list maybe-path)]
                     #:when (path-literal? p))
                 (push-path p))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/typed/provide
 (submod "." unsafe)
 [handbook-metainfo (-> Part (Values String (Listof String)))]
 [handbook-scripts (-> Part Symbol (Listof Path))])
