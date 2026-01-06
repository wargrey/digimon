#lang typed/racket/base

(require digimon/struct)

(struct Phantom ())
(struct Phantom:Pure Phantom ())
(struct Phantom:Info Phantom ([datum : Index]))

(define-struct matter : Dia-Block-Style
  #:forall ([type : T]
            [subtype : S #:+ Natural])
  ([opacity : Real 1.0])
  #:transparent)

(define m : (Dia-Block-Style Phantom Index)
  ((inst make-matter Phantom Index) #:type (Phantom:Pure) #:subtype 1))

(remake-matter m #:opacity 0.5)
(remake-matter* m #:type (Phantom:Info 0) #:opacity 0.5)

(when (matter?? m Phantom? byte?)
  m)
