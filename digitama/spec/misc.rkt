#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")

(require "../../format.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-message : (case-> [(Listof Any) -> (Option String)]
                               [String (Listof Any) -> String])
  (case-lambda
    [(argl)
     (and (pair? argl) (string? (car argl))
          (spec-message (car argl) (cdr argl)))]
    [(fmt argl) (~string fmt argl)]))

(define spec-location : (-> Syntax (Option srcloc))
  (lambda [stx]
    (let ([src (syntax-source stx)]
          [line (syntax-line stx)]
          [column (syntax-column stx)])                        
      (and (or (path? src) (path-string? src)) line column
           (srcloc src line column (syntax-position stx) (syntax-span stx))))))

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
