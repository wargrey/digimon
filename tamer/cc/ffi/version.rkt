#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-version (digimon-ffi-lib "version"))
  
  (define-version stdc_version_plus (_fun -> _long)))

(require/typed/provide
 (submod "." unsafe)
 [stdc_version_plus (-> Natural)])
