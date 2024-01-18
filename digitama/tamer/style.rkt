#lang racket/base

(provide (all-defined-out))
(provide (rename-out [plain placeholder-style]))

(require scribble/core)
(require scribble/html-properties)
(require scribble/latex-properties)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define noncontent-style (make-style #false '(unnumbered reverl no-index)))
(define grouper-style (make-style #false '(grouper)))
(define subsub*toc-style (make-style #false '(toc)))
(define subtitle-style (make-style "large" null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-title-style
  (lambda [name props ext-properties tamer-resource-files modname]
    (make-style name
                (foldl (λ [resrcs properties]
                         (append properties
                                 (filter-map (λ [tamer.res] (and (file-exists? tamer.res) ((car resrcs) tamer.res)))
                                             (tamer-resource-files modname (cdr resrcs)))))
                       (cond [(or (null? ext-properties) (void? ext-properties)) (if (list? props) props (list props))]
                             [(list? ext-properties) (if (list? props) (append props ext-properties) (cons props ext-properties))]
                             [else (if (list? props) (append props (list ext-properties)) (list props ext-properties))])
                       (list (cons make-css-addition "tamer.css")
                             (cons make-tex-addition "tamer.tex")
                             (cons make-js-addition "tamer.js")
                             (cons make-css-style-addition "tamer-style.css")
                             (cons make-js-style-addition "tamer-style.js"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-prefab-style
  (lambda properties
    (make-style #false
                (filter symbol? properties))))

(define style-attach-property
  (lambda [s prop . others]
    (make-style (style-name s)
                (if (null? others)
                    (cons prop (style-properties s))
                    (append (cons prop others) (style-properties s))))))

(define style-property-compose
  (lambda [props c prop?]
    (cons c (filter-not prop? props))))

(define style-merge-property
  (lambda [master slaver]
    (cond [(not master) slaver]
          [(not slaver) master]
          [else (make-style (style-name master)
                            (remove-duplicates
                             (append (style-properties master)
                                     (style-properties slaver))))])))
