#lang typed/racket/base

(provide (all-defined-out))
(provide unsafe-fx+ unsafe-fx- unsafe-fx* unsafe-fxquotient unsafe-fxremainder)
(provide unsafe-fxand unsafe-fxior unsafe-fxlshift unsafe-fxrshift unsafe-fxmax unsafe-fxmin)
(provide unsafe-fl->fx unsafe-fx->fl unsafe-flfloor unsafe-flceiling unsafe-flround unsafe-fltruncate)

(require typed/racket/unsafe)
(require (except-in racket/unsafe/ops
                    unsafe-vector*-ref unsafe-vector*-set!
                    unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-copy!))

(require "debug/ops.rkt")
(provide (all-from-out "debug/ops.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Racket `bitwise-not` always returns a negative number for natural.
; The safe version would be `(~n) & #xFFFF` or `(n & #xFFFF) ^ #xFFFF`

(define unsafe-uint16-not : (-> Integer Index)
  (lambda [n]
    (unsafe-idxxor n #xFFFF)))

(define unsafe-uint32-not : (-> Integer Index)
  (lambda [n]
    (unsafe-idxxor n #xFFFFFFFF)))
