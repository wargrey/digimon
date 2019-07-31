#lang typed/racket/base

(provide (all-defined-out))

(define spec-message : (-> (Listof Any) (Option String))
  (lambda [argl]
    (and (pair? argl) (string? (car argl))
         (cond [(null? (cdr argl)) (car argl)]
               [else (apply format (car argl) (cdr argl))]))))

(define spec-location : (-> Syntax (Option (List Path-String Positive-Integer Natural)))
  (lambda [stx]
    (let ([src (syntax-source stx)]
          [line (syntax-line stx)]
          [column (syntax-column stx)])                        
      (and (or (path? src) (path-string? src)) line column
           (list src line column)))))
