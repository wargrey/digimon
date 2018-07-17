#lang racket/base

(provide (all-defined-out))
(provide (rename-out [compose* >>=] [sequence* >=>]))

(define-struct monad (value state) #:mutable)

;; compose : (monad-of 'a) ('a -> (monad-of 'b)) -> (monad-of 'b)
(define (compose comp build-comp)
  (lambda (seed0)
    (let* ((seed1 (comp seed0))
           (value (monad-value seed1)))
      ((build-comp value) seed1))))

;; compose*: (monad-of 'a) ('a -> (monad-of 'b)) ... -> (monad-of 'b)
(define (compose* monad . actions)
  (if (null? actions)
      monad
      (compose monad
               (lambda (value)
                 (apply
                  compose*
                  ((car actions) value)
                  (cdr actions))))))

;; sequence : (monad-of 'a) (monad-of 'b) -> (monad-of 'b)
(define (sequence monad-a monad-b)
  (compose monad-a (lambda (v) monad-b)))

(define (sequence* monad . monads)
  (if (null? monads)
      monad
      (sequence monad
                (apply
                 sequence*
                 (car monads)
                 (cdr monads)))))
