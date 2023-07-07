#lang typed/racket/base

(provide (all-defined-out))

(require "../../../struct.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct c:image : C:Image
  ([symbol-names : (Listof String) null]
   [raw : Any #false])
  #:transparent)
