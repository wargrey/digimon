#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-filter (digimon-ffi-lib "filter"))

  (define-filter stdc_version (_fun -> _long))
  (define-filter stdc_version_filter (_fun -> _double)))

(require/typed/provide
 (submod "." unsafe)
 [stdc_version (-> Natural)]
 [stdc_version_filter (-> Nonnegative-Flonum)])
