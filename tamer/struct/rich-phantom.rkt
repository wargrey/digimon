#lang typed/racket/base

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct style : Style
  #:forall ([type : T])
  ([opacity : Real 1.0])
  #:transparent)

(struct rich-phantom
  ([tag : Symbol])
  #:type-name Rich-Phantom
  #:transparent)

(define-phantom-struct phantom:style : Phantom:Style #:for style
  ([opacity : Real 0.8])
  #:metadata
  ([tag : (Option Symbol) #false]))

(define-phantom-struct rich:phantom:style : Rich:Phantom:Style #:as rich-phantom #:for style
  ([opacity : Real 0.8])
  #:metadata
  ([tag : Symbol 'Class]))

(default-phantom:style)
(make-phantom:style #:tag 'derived #:opacity 1.0)

(default-rich:phantom:style)
(make-rich:phantom:style #:tag 'alias #:opacity 1.0)
