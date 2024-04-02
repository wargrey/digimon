#lang typed/racket/base

(provide (all-defined-out))

(require "../minimal/string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:spec exn:fail:user () #:constructor-name make-exn:spec)

(define make-spec-exn : (-> String Any * exn:spec)
  (lambda [msgfmt . argl]
    (make-exn:spec (~string msgfmt argl)
                   (current-continuation-marks))))

(define spec-throw : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (apply make-spec-exn msgfmt argl))))
