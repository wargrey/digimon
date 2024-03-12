#lang typed/racket/base

(provide (all-defined-out))

(require racket/promise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct toolchain
  ([program : (Promise (Option Path))]
   [option-layout : (Listof (U Symbol String))]
   [env : (U False Environment-Variables (-> Environment-Variables))])
  #:constructor-name abstract-toolchain
  #:type-name Tool-Chain
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define toolchain-promise-filter : (All (TC) (-> (Option (∩ TC Tool-Chain)) (Option TC)))
  (lambda [tc]
    (and tc
         (force (toolchain-program tc))
         tc)))
