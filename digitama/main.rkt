#lang typed/racket

(define-syntax (require/provide stx)
  (syntax-case stx []
    [(_ spec ...)
     #'(begin (provide (all-from-out spec)) ...
              (require spec) ...)]))

(require/provide racket/flonum racket/fixnum
                 "../system.rkt" "sugar.rkt")

(require/provide "../uuid.rkt" "../format.rkt"
                 "../timer.rkt" "../network.rkt")

(require/provide "../ffi.rkt" "../openssl.rkt")
