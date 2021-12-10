#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require "ffi/nested.rkt")
(require "ffi/filter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "C" #:do
            (describe "shared object for FFI" #:do
                      (context "nested" #:do
                               (describe random_seed #:do
                                         (it "should return an integer" #:do
                                             (expect-satisfy exact-integer? (random_seed)))))
                      (context "filter" #:do
                               (describe random_filter #:do
                                         (it "should return a double flonum" #:do
                                             (expect-satisfy flonum? (random_filter))))))))
  




(module+ main
  (void (spec-prove prelude)))
