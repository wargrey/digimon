#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require "ffi/nested.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "C Shared Objects" #:do
            (context "nested" #:do
                     (describe random_seed #:do
                               (it "should return an integer" #:do
                                   (expect-satisfy exact-integer? (random_seed)))))))





(module+ main
  (void (spec-prove prelude)))
