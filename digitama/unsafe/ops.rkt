#lang typed/racket/base

(provide (all-defined-out))
(provide unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-copy!)
(provide unsafe-vector*-ref unsafe-vector*-set!)
(provide unsafe-fxand unsafe-fxior unsafe-fxremainder)
(provide (rename-out [unsafe-fx+ unsafe-idx+]
                     [unsafe-fx- unsafe-idx-]
                     [unsafe-fxxor unsafe-idxxor]
                     [unsafe-fxlshift unsafe-idxlshift]
                     [unsafe-fxrshift unsafe-idxrshift]))


(require racket/unsafe/ops)
(require typed/racket/unsafe)

(unsafe-require/typed
 racket/unsafe/ops
 [unsafe-fx+ (-> Natural Natural Index)]
 [unsafe-fx- (case-> [Byte Byte -> Byte]
                     [Zero Negative-Fixnum -> Index]
                     [Natural Natural -> Index])]
 [unsafe-fxxor (-> Integer Natural Index)]
 [unsafe-fxlshift (-> Natural Fixnum Index)]
 [unsafe-fxrshift (-> Natural Byte Index)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Racket `bitwise-not` always returns a negative number for natural.
; The safe version would be `(~n) & #xFFFF` or `(n & #xFFFF) ^ #xFFFF`

(define unsafe-uint16-not : (-> Integer Index)
  (lambda [n]
    (unsafe-fxxor n #xFFFF)))

(define unsafe-uint32-not : (-> Integer Index)
  (lambda [n]
    (unsafe-fxxor n #xFFFFFFFF)))
