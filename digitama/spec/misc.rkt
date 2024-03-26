#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")

(require "../../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-message : (case-> [(Listof Any) -> (Option String)]
                               [String (Listof Any) -> String])
  (case-lambda
    [(argl)
     (and (pair? argl) (string? (car argl))
          (spec-message (car argl) (cdr argl)))]
    [(fmt argl) (~string fmt argl)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-format/octet : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(byte? v) (~binstring v 8)]
          [(integer? v) (~binstring v)]
          [(bytes? v) (~binstring v)]
          [else (fallback v)])))

(define spec-format/bin : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(byte? v) (~binstring v)]
          [(integer? v) (~binstring v)]
          [(bytes? v) (~binstring v)]
          [else (fallback v)])))

(define spec-format/hex : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(byte? v) (byte->hexstring v)]
          [(integer? v) (~hexstring v)]
          [(bytes? v) (~hexstring v)]
          [else (fallback v)])))
