#lang racket

(provide (except-out (all-from-out racket) #%app))
(provide #%module-begin #%datum #%top)
(provide (rename-out [wisemon-app #%app]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (wisemon-app stx)
  (syntax-case stx []
    [(_ proc argl ...)
     #'(begin (displayln (list 'proc argl ...))
              (#%app proc argl ...))]))
