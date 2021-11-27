#lang typed/racket

(require typed/racket/unsafe)

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
