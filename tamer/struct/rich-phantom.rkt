#lang typed/racket/base

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct style : Style
  #:forall ([type : T])
  ([opacity : Real 1.0])
  #:transparent)

(define-phantom-struct phantom:style : Phantom:Style #:for style
  ([opacity : Real 0.8])
  #:metadata
  ([tag : (Option Symbol) #false]))

(default-phantom:style)
(make-phantom:style #:tag 'rich #:opacity 1.0)
