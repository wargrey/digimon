#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-version (digimon-ffi-lib "version" #:global? #true))
  
  (define-version stdc_version_plus (_fun -> _long))
  (define-version conflict_datum (_fun -> _int)))

(require/typed/provide
 (submod "." unsafe)
 [stdc_version_plus (-> Natural)]
 [conflict_datum (-> Integer)])


(module+ main
  (stdc_version_plus)
  (conflict_datum))
