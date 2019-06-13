#lang typed/racket/base

(provide (all-defined-out))

(define-type CC-Options (U 'flags 'macros 'includes 'infile 'outfile))

(define-type CC-CPP-Macros (-> Symbol (Listof String)))
(define-type CC-Flags (-> Symbol (Listof String)))
(define-type CC-Includes (-> Symbol (Listof String)))

(struct cc
  ([basename : Symbol]
   [option-layout : (Listof (U CC-Options String))]
   [macros : CC-CPP-Macros]
   [flags : CC-Flags]
   [includes : CC-Includes])
  #:constructor-name make-cc
  #:type-name CC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-database : (HashTable Symbol CC) (make-hasheq))

(define c-register-compiler : (-> Symbol CC Void)
  (lambda [name cc]
    (hash-set! cc-database name cc)))
