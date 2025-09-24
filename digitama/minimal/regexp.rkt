#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bs-regexp? : (-> Any Boolean : (U Regexp Byte-Regexp))
  (lambda [v]
    (or (byte-regexp? v)
        (regexp? v))))

(define regexp-like? : (-> Any Boolean : (U Regexp Byte-Regexp String))
  (lambda [v]
    (or (byte-regexp? v)
        (regexp? v)
        (string? v))))
