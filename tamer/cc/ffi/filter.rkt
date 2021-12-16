#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-nested (digimon-ffi-lib "filter"))

  (define-nested random_seed (_fun -> _int))
  (define-nested random_filter (_fun -> _double)))

(require/typed/provide
 (submod "." unsafe)
 [random_seed (-> Natural)]
 [random_filter (-> Nonnegative-Flonum)])
