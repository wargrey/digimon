#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Make-Phony (-> String Info-Ref Any))

(struct phony
  ([make : Make-Phony]
   [description : String])
  #:constructor-name make-phony
  #:type-name Phony)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define phony-database : (HashTable Symbol Phony) (make-hasheq))

(define wisemon-register-phony : (-> Symbol Make-Phony String Void)
  (lambda [name phony desc]
    (unless (hash-has-key? phony-database name)
      (hash-set! phony-database name (make-phony phony desc)))))
