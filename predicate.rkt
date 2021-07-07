#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-listof? : (All (T) (-> Any (-> Any Boolean : T) Boolean : #:+ (Listof T) #:- (! (Listof T))))
  (lambda [v ?]
    (and (list? v)
         (andmap ? v))))

(define is-listof+? : (All (T) (-> Any (-> Any Boolean : T) Boolean : #:+ (Pairof T (Listof T)) #:- (! (Pairof T (Listof T)))))
  (lambda [v ?]
    (and (list? v)
         (pair? v)
         (andmap ? v))))
