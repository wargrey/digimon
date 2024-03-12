#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require "ffi/filter.rkt")
(require "ffi/version.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "C" #:do
            (describe "shared object for FFI" #:do
              (context "nested" #:do
                (describe stdc_version #:do
                  (it "should return a long integer" #:do
                    (expect-satisfy exact-integer? (stdc_version))
                    (expect-> (stdc_version) 198400))))
              (context "filter" #:do
                (describe stdc_version_filter #:do
                  (it "should return a double flonum" #:do
                    (expect-satisfy flonum? (stdc_version_filter))
                    (displayln (stdc_version_filter))
                    (expect-> (stdc_version_filter) 1984.0))))))
  (describe "C++" #:do
    (describe "shared object for FFI" #:do
      (context "version" #:do
        (describe stdc_version_plus #:do
          (it "should return a long integer" #:do
            (expect-satisfy exact-integer? (stdc_version_plus))
            (expect-> (stdc_version_plus) 198400)))))))





(module+ main
  (void (spec-prove prelude)))
