#lang typed/racket/base

(provide (all-from-out typed/racket/class))

(require typed/racket/unsafe)
(require typed/racket/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module untyped racket/base
  (provide (all-defined-out))

  (require racket/class)

  (define typeof?
    (lambda [obj type%]
      (is-a? obj type%))))

(unsafe-require/typed/provide
 (submod "." untyped)
 [typeof? (All (a) (-> Any a Boolean : #:+ (Instance a) #:- (! (Instance a))))])
