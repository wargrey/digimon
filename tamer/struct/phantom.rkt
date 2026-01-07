#lang typed/racket/base

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct Phantom ())
(struct Phantom:Pure Phantom ())
(struct Phantom:Info Phantom ([datum : Index]))

(define-struct matter : Matter
  #:forall ([type : T]
            [subtype : S #:+ Natural])
  ([datum : Real 1.0])
  #:transparent)

(define m : (Matter Phantom Index)
  ((inst make-matter Phantom Index) #:type (Phantom:Pure) #:subtype 1))

(remake-matter m #:datum 0.5)
(remake-matter* m #:type (Phantom:Info 0) #:datum 0.5)

(when (matter?? m Phantom? byte?)
  (matter-type m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct style : Style
  #:forall ([type : T])
  ([opacity : Real 1.0])
  #:transparent)

(define-phantom-struct phantom:style : Phantom:Style #:-> Phantom #:for style
  ([opacity : Real 0.8]))

(default-phantom:style)
(make-phantom:style #:opacity 1.0)
