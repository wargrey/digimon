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

(define disjoin? : (All (a b c) (case-> [Any (-> Any Boolean : a) (-> Any Boolean : b) -> Boolean : #:+ (U a b) #:- (! (U a b))]
                                        [Any (-> Any Boolean : a) (-> Any Boolean : b) (-> Any Boolean : c) -> Boolean : #:+ (U a b c) #:- (! (U a b c))]))
  (case-lambda
    [(v p1? p2?) (or (p1? v) (p2? v))]
    [(v p1? p2? p3?) (or (p1? v) (p2? v) (p3? v))]))
