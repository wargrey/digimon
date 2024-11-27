#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)

(require (prefix-in c:: "ffi/filter.rkt"))
(require (prefix-in cpp:: "ffi/version.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "C" #:do
            (describe "shared object for FFI" #:do
              (context "nested" #:do
                (describe c::stdc_version #:do
                  (it "should return a long integer" #:do
                    (expect-satisfy exact-integer? (c::stdc_version))
                    (expect-> (c::stdc_version) 198400))))
              (context "filter" #:do
                (describe c::stdc_version_filter #:do
                  (it "should return a double flonum" #:do
                    (expect-satisfy flonum? (c::stdc_version_filter))
                    (displayln (c::stdc_version_filter))
                    (expect-> (c::stdc_version_filter) 1984.0))))))
  (describe "C++" #:do
    (describe "shared object for FFI" #:do
      (context "version" #:do
        (describe cpp::stdc_version_plus #:do
          (it "should return a long integer" #:do
            (expect-satisfy exact-integer? (cpp::stdc_version_plus))
            (expect-> (cpp::stdc_version_plus) 198400))))))

  (describe "Conflicting Datum" #:do
    (it "should return 1 for C version" #:do
      (expect-= (c::conflict_datum) 1))
    (it "should return 2 for C++ version" #:do
      (expect-= (cpp::conflict_datum) 2))
    (it "should return different values" #:do
      (expect-not-eq (c::conflict_datum) (cpp::conflict_datum)))))





(module+ main
  (void (spec-prove prelude)))
