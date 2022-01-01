#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define os : Symbol (system-type 'os))

(struct toolchain
  ([program : Path]
   [option-layout : (Listof (U Symbol String))]
   [env : (U False Environment-Variables (-> Environment-Variables))])
  #:constructor-name abstract-toolchain
  #:type-name Tool-Chain
  #:transparent)
