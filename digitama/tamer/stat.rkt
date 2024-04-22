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
  
  (require "style.rkt")
  (require "backend.rkt")

  (require "../unicode.rkt")
  (require "../minimal/format.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      
      (get-field metrics stat)))

  (define handbook-part-stat
    (lambda [part type include-section?]
      ;;; NOTICE
      ; this cannot be replaced by
      ;   just invoking `render-part` with
      ;   another renderer's `r:metrics`
      (define docs (list part))
      (define names (list #false))
      
      (define stat (make-object scrbl-stat% type))

      (define t:metrics (send stat traverse docs names))
      (define c:metrics (send stat collect  docs names t:metrics))
      (define r:metrics (send stat resolve  docs names c:metrics))
      
      (send stat render docs names r:metrics)
      (send stat done)

      (let* ([all-stats (get-field metrics stat)]
             [hstats (hash-ref all-stats 'head string-word-count-start)]
             [bstats (hash-ref all-stats 'body string-word-count-start)]
             [wstats (if (not include-section?) bstats (word-statistics+ hstats bstats))])
        (values (word-statistics-asian bstats)
                (word-statistics-letter-word bstats)
                (word-statistics-emoji wstats)
                (- (word-statistics-char wstats)
                   (word-statistics-whitespace wstats))))))

  (define handbook-display-metrics
    (lambda [dtrace level metrics]
      (define ordered-groups '(head body))
      (define ordered-keys '(word asian emoji char-ws char+ws))
      (define key-maps
        #hasheq((asian . "Asian Characters")
                (word . "Letter Words")
                (char-ws . "Net Characters")
                (char+ws . "All Characters")
                (emoji . "Emojis")
                (head . "Title")
                (body . "Body")))

      (for ([group (in-list ordered-groups)])
        (define wstats (hash-ref metrics group string-word-count-start))
        (define type (hash-ref key-maps group (λ [] group)))
        (define total
          (+ (word-statistics-asian wstats)
             (word-statistics-letter-word wstats)
             (word-statistics-emoji wstats)))
        
        (when (> total 0)
          (dtrace level "~a: Words: ~a" type (~thousand total)))
        
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
            (dtrace level "~a: ~a: ~a" type
                    (hash-ref key-maps key (λ [] key))
                    (~thousand datum)))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define scrbl-stat%
    (class render%
      (super-new [dest-dir #false])
      
      (init-field render-type)
      (init-field [/dev/stdout (current-output-port)])
      
      (field [metrics (make-hasheq)])
      
      (inherit render-block)

      (define default-word-group (make-parameter 'body))
      (define current-disable-word-boundary (make-parameter #false))
      
      (define/override (current-render-mode) (list render-type handbook-stat-render-name))
      (define/override (get-suffix) '#:who-cares #".wc")
      (define/override (install-file fn [content #f] #:private-name? [private-name? #t]) (void))
      (define/override (table-of-contents part ri) empty-block)
      (define/override (local-table-of-contents part ri) empty-block)

      (define/override (get-substitutions)
        '((#rx"---" "\U2014")
          (#rx"--" "\U2013")
          (#rx"``" "\U201C")
          (#rx"''" "\U201D")
          (#rx"'" "\U2019")))

      (define/override (render-part self ri)
        (parameterize ([default-word-group 'head])
          (super render-part self ri)))

      (define/override (render-paragraph self parent ri)
        (parameterize ([default-word-group 'body]
                       [current-disable-word-boundary #true])
          (begin0 (super render-paragraph self parent ri)
                  (temporarily-done-for 'body))))

      (define/override (render-table self parent ri starting-item?)
        (for/list ([cell (in-list (apply append (table-blockss self)))]
                   #:when (block? cell))
          (render-block cell parent ri starting-item?)))

      (define/override (render-auxiliary-table self parent ri)
        (render-table self parent ri #false))
    
      (define/override (render-content self parent ri)
        ; we don't deal with elements of 'newline and 'hspace on purpose
        (cond [(string? self) (word-count self parent)]
              [(convertible? self) (word-count-convertible self parent)]
              [(symbol? self) (word-count-symbol self parent)])
        (super render-content self parent ri))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define/public (done)
        (for ([key (in-list (hash-keys metrics))])
          (temporarily-done-for key)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (word-count text parent [group (default-word-group)])
        (define wstat (hash-ref metrics group string-word-count-start))
        (define wstat++ (string-word-count-iterate text wstat))

        (hash-set! metrics group
                   (cond [(current-disable-word-boundary) wstat++]
                         [else (string-word-count-done wstat++)])))

      (define (word-count-symbol self part [group (default-word-group)])
        (case self
          [(mdash) (word-count "\U2014" part group)]
          [(ndash) (word-count "\U2013" part group)]
          [(ldquo) (word-count "\U201C" part group)]
          [(rdquo) (word-count "\U201D" part group)]
          [(lsquo) (word-count "\U2018" part group)]
          [(rsquo) (word-count "\U2019" part group)]
          [(lang) (word-count ">" part group)]
          [(rang) (word-count "<" part group)]
          [(rarr) (word-count "->" part group)]
          [(nbsp) (word-count "\uA0" part group)]
          [(prime) (word-count "'" part group)]
          [(alpha) (word-count "\u03B1" part group)]
          [(infin) (word-count "\u221E" part group)]
          [else (word-count "." part group)]))
      
      (define (word-count-convertible self part [group (default-word-group)])
        (define text (convert self 'text))

        (when (string? text)
          (word-count text part group)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (temporarily-done-for key)
        (when (hash-has-key? metrics key)
          (hash-set! metrics key
                     (string-word-count-done
                      (hash-ref metrics key))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/typed/provide
 (submod "." unsafe)
 [handbook-stats (-> Part Symbol (HashTable Symbol Word-Statistics))]
 [handbook-display-metrics (-> (-> Symbol String Any * Any) Symbol (HashTable Symbol Word-Statistics) Void)])
