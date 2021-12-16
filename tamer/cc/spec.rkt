#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require "ffi/filter.rkt")
(require "ffi/seed.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "C" #:do
            (describe "shared object for FFI" #:do
                      (context "nested" #:do
                               (describe random_seed #:do
                                         (it "should return a small integer" #:do
                                             (expect-satisfy exact-integer? (random_seed))
                                             (expect-< (random_seed) 64))))
                      (context "filter" #:do
                               (describe random_filter #:do
                                         (it "should return a small double flonum" #:do
                                             (expect-satisfy flonum? (random_filter))
                                             (expect-< (random_filter) 64.0))))))
  (describe "C++" #:do
            (describe "shared object for FFI" #:do
                      (context "seed" #:do
                               (describe random_seed_plus #:do
                                         (it "should return a large integer" #:do
                                             (expect-satisfy exact-integer? (random_seed_plus))
                                             (expect->= (random_seed_plus) 64)))))))
  




(module+ main
  (void (spec-prove prelude)))
