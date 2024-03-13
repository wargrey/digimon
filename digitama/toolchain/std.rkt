#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Standard-Version (U Index Symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-default-standard-version : CC-Standard-Version 2017)

(define cc-standard-version? : (-> Any Boolean : CC-Standard-Version)
  (lambda [v]
    (or (index? v)
        (symbol? v))))

(define cc-standard-short-version->string : (-> Index String)
  (lambda [v]
    (cond [(< v 10) (format "0~a" v)]
          [(> v 100) (cc-standard-short-version->string (remainder v 100))]
          [else (format "~a" v)])))
