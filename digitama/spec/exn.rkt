#lang typed/racket/base

(provide (all-defined-out))

(require "../../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:spec exn:fail:user () #:constructor-name make-exn:spec)

(define spec-throw : (-> String Any * Nothing)
  (lambda [msgfmt . argl]
    (raise (make-exn:spec (~string msgfmt argl)
                          (current-continuation-marks)))))
