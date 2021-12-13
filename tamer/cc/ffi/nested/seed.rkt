#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-nested (digimon-ffi-lib "seed"))
  
  (define-nested random_seed (_fun -> _int)))

(require/typed/provide
 (submod "." unsafe)
 [random_seed (-> Natural)])
