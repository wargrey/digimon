#lang racket/base

(provide (all-defined-out))

(require scribble/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For `scrbl.rkt`
(struct traverse-block/shadow traverse-block
  (block #| : (-> Block) |#)
  #:constructor-name make-shadow-traverse-block)

(struct traverse-element/shadow traverse-element
  (element #| : (-> Element) |#)
  #:constructor-name make-shadow-traverse-element)
