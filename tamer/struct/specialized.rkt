#lang typed/racket/base

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) id : (-> T T) values)

(define-struct #:forall (T) factory : Factory
  ([id : (-> T T) id])
  #:transparent)

(define-struct/parameter #:specialized (Symbol) symbol-factory : Symbol-Factory #:as factory
  ([id : (-> Symbol Symbol) id]))

(define f (make-symbol-factory))
(remake-symbol-factory f #:id (λ [[v : Symbol]] v))

(define p : (symbol-factory Index) (unsafe-symbol-factory values))
; (remake-symbol-factory p #:id 1) <- type error
