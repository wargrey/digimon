#lang racket/base

(provide (all-defined-out))
(provide (rename-out [plain placeholder-style]))

(require scribble/core)
(require scribble/html-properties)
(require scribble/latex-properties)

(require racket/list)
(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define noncontent-style (make-style #false '(unnumbered reverl no-index)))
(define grouper-style (make-style #false '(grouper)))
(define subsub*toc-style (make-style #false '(toc)))
(define subtitle-style (make-style "large" null))

(define empty-block (make-paragraph plain null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-title-style
  (lambda [name props ext-properties tamer-resource-files scrbl others]
    (define dirname (path-only scrbl))
    (define basename (path->string (path-replace-extension (file-name-from-path scrbl) #"")))
    (define additions (filter values others))

    (define initial-props
      (cond [(or (null? ext-properties) (void? ext-properties)) (if (list? props) props (list props))]
            [(list? ext-properties) (if (list? props) (append props ext-properties) (cons props ext-properties))]
            [else (if (list? props) (append props (list ext-properties)) (list props ext-properties))]))

    (define master-properties
      (foldl (λ [resrcs properties]
               (append properties
                       (filter-map (λ [tamer.res] (and (file-exists? tamer.res) ((car resrcs) tamer.res)))
                                   (tamer-resource-files dirname basename (cdr resrcs)))))
             (if (null? additions) initial-props (append additions initial-props))
             (list (cons make-css-addition ".css")
                   (cons make-tex-addition ".tex")
                   (cons make-js-addition ".js")
                   (cons make-css-style-addition "-style.css")
                   (cons make-js-style-addition "-style.js"))))

    (make-style name master-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-style
  (lambda [s]
    (cond [(style? s) s]
          [(or (string? s) (symbol? s)) (make-style style null)]
          [(list? s) (and (pair? s) (make-style #false s))]
          [(not s) (make-style #false (list s))]
          [else #false])))

(define handbook-element
  (lambda [s self]
    (define maybe-style (handbook-style s))
    (cond [(style? style) (make-element maybe-style self)]
          [else self])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-remove-style-name
  (lambda [s]
    (make-style #false (style-properties s))))

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
