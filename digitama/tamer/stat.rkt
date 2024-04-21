#lang typed/racket/base

(provide (all-defined-out))

(require "typed.rkt")
(require "../unicode.rkt")

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

  (require "../unicode.rkt")
  (require "../minimal/format.rkt")
  
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
  
  (define handbook-stats
    (lambda [part type]
      ; the required arguments of `render`
      (define docs (list part))
      (define names (list #false))
      
      (define stat (make-object scrbl-stat% type))
      
      (define t:metrics (send stat traverse docs names))
      (define c:metrics (send stat collect  docs names t:metrics))
      (define r:metrics (send stat resolve  docs names c:metrics))
      
      (send stat render docs names r:metrics)
      
      (values (hash-keys (get-field scripts stat))
              (get-field metrics stat))))

  (define handbook-display-metrics
    (lambda [dtrace level metrics]
      (define ordered-keys '(char-ws char+ws word asian emoji))
      (define key-maps
        #hasheq((asian . "Asian Characters")
                (word . "Letter Words")
                (char-ws . "Net Characters")
                (char+ws . "All Characters")
                (emoji . "Emojis")))

      (define wstats (hash-ref metrics 'default string-word-count-start))
      (define total
        (+ (word-statistics-asian wstats)
           (word-statistics-letter-word wstats)
           (word-statistics-emoji wstats)))
      
      (when (> total 0)
        (dtrace level "Words: ~a" (~thousand total)))
      
      (for ([key (in-list ordered-keys)])
        (define datum
          (case key
            [(asian) (word-statistics-asian wstats)]
            [(word) (word-statistics-letter-word wstats)]
            [(char+ws) (word-statistics-char wstats)]
            [(char-ws) (- (word-statistics-char wstats) (word-statistics-whitespace wstats))]
            [(emoji) (word-statistics-emoji wstats)]
            [else 0]))
        
        (when (> datum 0)
          (dtrace level "~a: ~a"
                  (hash-ref key-maps key (λ [] key))
                  (~thousand datum))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define scrbl-stat%
    (class render%
      (super-new [dest-dir #false])
      
      (init-field render-type)
      (init-field [/dev/stdout (current-output-port)])
      
      (field [scripts (make-hash)]
             [metrics (make-hasheq)])
      
      (inherit render-block)
      
      (define/override (current-render-mode) (list render-type handbook-stat-render-name))
      (define/override (get-suffix) '#:who-cares #".wc")
      (define/override (install-file fn [content #f] #:private-name? [private-name? #t]) (void))
      (define/override (table-of-contents part ri) empty-block)
      (define/override (local-table-of-contents part ri) empty-block)
      
      (define/override (render-part self ri)
        (extract-style-path (part-style self))
        (word-count (content->string (part-title-content self) this self ri) self)
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
        (cond [(string? self) (word-count self parent)]
              [(element? self)
               (extract-style-path* (element-style self))
               (when (image-element? self)
                 (push-path* (image-element-path self)
                             (image-element-suffixes self)))]
              [(multiarg-element? self)
               (extract-style-path* (multiarg-element-style self))]
              [(convertible? self)
               (convertible-script-paths self)
               (word-count-convertible self parent)]
              [(symbol? self) (word-count-symbol self parent)])
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
                 (push-path p))]))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define wc-disabled-parts (make-hash))
      
      (define (word-count text parent [group 'default])
        (unless (hash-ref! wc-disabled-parts parent
                           (λ [] #false))
          (hash-set! metrics group
                     (string-word-count-iterate text
                                                (hash-ref metrics group
                                                          string-word-count-start)))))

      (define (word-count-symbol self part [group 'default])
        (case self
          [(rarr) (word-count "->" part group)]
          [(nbsp) (word-count " " part group)]
          [else (word-count "." part group)]))
      
      (define (word-count-convertible self part [group 'default])
        (define text (convert self 'text))

        (when (string? text)
          (word-count text part group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/typed/provide
 (submod "." unsafe)
 [handbook-metainfo (-> Part (Values String (Listof String)))]
 [handbook-stats (-> Part Symbol (Values (Listof Path) (HashTable Symbol Word-Statistics)))]
 [handbook-display-metrics (-> (-> Symbol String Any * Any) Symbol (HashTable Symbol Word-Statistics) Void)])
