#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/cc/compiler.rkt")
(require "digitama/cc/linker.rkt")

(require "digitama/cc/toolchain/gcc.rkt")
(require "digitama/cc/toolchain/ld.rkt")
(require "digitama/cc/toolchain/msvc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-list-compilers : (-> (Listof Symbol))
  (lambda []
    (hash-keys cc-database)))

(define c-list-linkers : (-> (Listof Symbol))
  (lambda []
    (hash-keys ld-database)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c-list-compilers)
(c-list-linkers)
