#lang racket/base

(provide (all-defined-out))

(require racket/symbol)

(require scribble/manual)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-defstruct stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ struct-name (~or : #:) type-name (fields ...) [options ...] pre-flow ...)
     (with-syntax ([struct-name? (datum->syntax #'struct-name (string->symbol (format "~a?" (syntax-e #'struct-name))))])
       (syntax/loc stx (deftogether [(defthing #:kind "syntax" type-name struct-name?)
                       (defstruct* struct-name (fields ...) options ...)]
                         pre-flow ...)))]
    [(_ struct-name (fields ...) [options ...] pre-flow ...)
     (syntax/loc stx (defstruct* struct-name (fields ...) options ... pre-flow ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-indexed-keyword-element
  (lambda [keyword]
    (define content (racket '#,keyword))
    (index* (list (symbol->immutable-string keyword))
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
