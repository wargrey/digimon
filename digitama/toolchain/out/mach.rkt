#lang typed/racket/base

(provide (all-defined-out))

(require "image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mach-image? : (-> Input-Port Boolean)
  (lambda [/dev/stdin]
    #false))

(define read-mach-image : (-> Input-Port C:Image)
  (lambda [/dev/stdin]
    (make-c:image)))
