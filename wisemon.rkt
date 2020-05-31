#lang typed/racket/base

(provide (all-defined-out))

(define-type Wisemon-Specification (Listof Wisemon-Spec))

(struct wisemon-spec
  ([target : (U Path (Listof Path))]
   [prerequisites : (Listof Path)]
   [recipe : (-> Path Void)])
  #:type-name Wisemon-Spec
  #:transparent)
