#lang typed/racket/base

(provide (all-defined-out))

(define-type LD-Options (U 'flags 'libpath 'libraries 'infiles 'outfile))

(struct ld
  ([basename : Symbol]
   [option-layout : (Listof (U LD-Options String))])
  #:constructor-name make-ld
  #:type-name LD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-database : (HashTable Symbol LD) (make-hasheq))

(define c-register-linker : (-> Symbol LD Void)
  (lambda [name ld]
    (hash-set! ld-database name ld)))

