#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (N) ?list : (case-> [(Option N) -> (U Null (List N))]
                                     [(Option N) (-> N Boolean) -> (U Null (List N))])
  (case-lambda
    [(v) (if (not v) null (list v))]
    [(v ?) (if (and v (? v)) (list v) null)]))

(define #:forall (N) ?cons : (case-> [(Option N) (Listof N) -> (Listof N)]
                                     [(Option N) (Listof N) (-> N Boolean) -> (Listof N)])
  (case-lambda
    [(v ls) (if (not v) ls (cons v ls))]
    [(v ls ?) (if (and v (? v)) (cons v ls) ls)]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (N) vector->4:values : (-> (Vectorof N) N (Values N N N N))
  (lambda [vs defval]
    (define size (vector-length vs))
    
    (cond [(zero? size) (values defval defval defval defval)]
          [(= size 1) (let ([top (vector-ref vs 0)]) (values top top top top))]
          [(= size 2) (let ([top (vector-ref vs 0)] [right (vector-ref vs 1)]) (values top right top right))]
          [(= size 3) (let ([top (vector-ref vs 0)] [right (vector-ref vs 1)] [bottom (vector-ref vs 2)]) (values top right bottom right))]
          [else (values (vector-ref vs 0) (vector-ref vs 1) (vector-ref vs 2) (vector-ref vs 3))])))

(define #:forall (T) vector->n:vector : (-> (Vectorof T) Natural T (Vectorof T))
  (lambda [src total supplement]
    (define size (vector-length src))

    (cond [(= size total) src]
          [(> size total) (vector-take src total)]
          [else (vector-append src (make-vector (- total size) supplement))])))

(define #:forall (T D) vector->n:vector* : (-> (Vectorof T) Natural D (-> T D) (Vectorof D))
  (lambda [src total supplement ->v]
    (for/vector : (Vectorof D) #:length total #:fill supplement
      ([datum (in-vector src)]) (->v datum))))

