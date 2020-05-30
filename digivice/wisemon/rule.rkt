#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/path)
(require racket/format)

(require typed/racket/unsafe)

(require "parameter.rkt")

(unsafe-require/typed/provide
 make
 [make/proc (-> Make-Specification (U String (Listof Path-String) (Vectorof Path-String)) Void)])

(unsafe-require/typed/provide
 "unsafe.rkt"
 [unsafe-wisemon-make (->* (Unsafe-Wisemon-Rules) ((Listof Path-String)) Void)]
 [unsafe-hack-rule (-> Wisemon-Rule Make-Rule)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Rule (List Path (Listof Path) (-> Path Void)))
(define-type Wisemon-Rules (Listof Wisemon-Rule))
(define-type Unsafe-Wisemon-Rule (List Path-String (Listof Path-String) Procedure))
(define-type Unsafe-Wisemon-Rules (Listof Unsafe-Wisemon-Rule))

(define-type Make-Rule (List Path (Listof Path) (-> Void)))
(define-type Make-Specification (Listof Make-Rule))
(define-type Unsafe-Make-Rule (List Path-String (Listof Path-String) Procedure))
(define-type Unsafe-Make-Specification (Listof Unsafe-Make-Rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Rules) ((Listof Path-String)) Void)
  (lambda [rules [targets null]]
    (unsafe-wisemon-make rules targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hack-rule : (-> Wisemon-Rule Make-Rule)
  (lambda [r]
    (unsafe-hack-rule r)))
