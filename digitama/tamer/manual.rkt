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
(define tamer-keywords
  (lambda [keyword [seq ", "] [before-last ", and "]]
    (if (symbol? keyword)
        (let ([content (racket '#,keyword)])
          (index* (list (symbol->string keyword)) (list content) content))
        (let* ([keywords (filter symbol? keyword)]
               [size (length keywords)])
          (for/list ([kw (in-list keyword)]
                     [idx (in-naturals 1)])
            (cond [(= idx 1) (tamer-keywords kw)]
                  [(= idx size) (list (or before-last seq) (tamer-keywords kw))]
                  [else (list seq (tamer-keywords kw))]))))))
