#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (N) list->4:values : (-> (Listof N) N (Values N N N N))
  (lambda [ls defval]
    (cond [(null? ls) (values defval defval defval defval)]
          [(null? (cdr ls)) (let ([top (car ls)]) (values top top top top))]
          [(null? (cddr ls)) (let ([top (car ls)] [right (cadr ls)]) (values top right top right))]
          [(null? (cdddr ls)) (let ([top (car ls)] [right (cadr ls)] [bottom (caddr ls)]) (values top right bottom right))]
          [else (values (car ls) (cadr ls) (caddr ls) (cadddr ls))])))

(define #:forall (T) list->n:vector : (case-> [(Pairof T (Listof T)) Natural -> (Vectorof T)]
                                              [(Listof T) Natural T -> (Vectorof T)])
  (case-lambda
    [(src total)
     (for/vector : (Vectorof T) #:length total #:fill (last src)
       ([datum (in-list src)]) datum)]
    [(src total supplement)
     (for/vector : (Vectorof T) #:length total #:fill supplement
       ([datum (in-list src)]) datum)]))

(define #:forall (T D) list->n:vector* : (case-> [(Pairof T (Listof T)) Natural (-> T D) -> (Vectorof D)]
                                                 [(Listof T) Natural D (-> T D) -> (Vectorof D)])
  (case-lambda
    [(src total ->v)
     (for/vector : (Vectorof D) #:length total #:fill (->v (last src))
       ([datum (in-list src)]) (->v datum))]
    [(src total supplement ->v)
     (for/vector : (Vectorof D) #:length total #:fill supplement
       ([datum (in-list src)]) (->v datum))]))
