#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require "ffi/nested.rkt")
(require "ffi/filter.rkt")
(require "ffi/seed.rkt")

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
                                             (expect-satisfy flonum? (random_filter)))))))
  (describe "C++" #:do
            (describe "shared object for FFI" #:do
                      (context "seed" #:do
                               (describe random_seed #:do
                                         (it "should return an integer" #:do
                                             (expect-satisfy exact-integer? (random_seed_plus))))))))
  




(module+ main
  (void (spec-prove prelude)))
