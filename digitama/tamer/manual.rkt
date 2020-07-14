#lang racket/base

(provide (all-defined-out))

(require scribble/manual)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-defstruct stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ struct-name (~or : #:) type-name (fields ...) [options ...] pre-flow ...)
     (with-syntax ([struct-name? (datum->syntax #'struct-name (string->symbol (format "~a?" (syntax-e #'struct-name))))])
       #'(deftogether [(defstruct* struct-name (fields ...) options ...)
                       (defthing #:kind "syntax" type-name struct-name?)]
           pre-flow ...))]
    [(_ struct-name (fields ...) [options ...] pre-flow ...)
     #'(defstruct* struct-name (fields ...) options ... pre-flow ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-indexed-keyword-element
  (lambda [keyword]
    (define content (racket '#,keyword))
    (index* (list (symbol->string keyword))
            (list content)
            content)))

(define tamer-keyword-element
  (lambda [keyword]
    (racket '#,keyword)))

(define tamer-indexed-keywords
  (lambda [keyword [seq ", "] [before-last ", and "]]
    (tamer-make-keywords tamer-indexed-keyword-element
                         keyword seq before-last)))

(define tamer-keywords
  (lambda [keyword [seq ", "] [before-last ", and "]]
    (tamer-make-keywords tamer-keyword-element
                         keyword seq before-last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-make-keywords
  (lambda [make-element keyword [seq ", "] [before-last ", and "]]
    (cond [(symbol? keyword) (make-element keyword)]
          [else (let* ([keywords (filter symbol? keyword)]
                       [size (length keywords)])
                  (for/list ([kw (in-list keyword)]
                             [idx (in-naturals 1)])
                    (cond [(= idx 1) (make-element kw)]
                          [(= idx size) (list (or before-last seq) (make-element kw))]
                          [else (list seq (make-element kw))])))])))
