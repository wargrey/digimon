#lang typed/racket/base

(provide (all-defined-out))

(provide unsafe-b+ unsafe-b* unsafe-brshift)
(provide unsafe-idx+ unsafe-idx- unsafe-idx* unsafe-idxxor unsafe-idxlshift unsafe-idxrshift)
(provide unsafe-vector*-ref unsafe-vector*-set! unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-copy!)

(require racket/unsafe/ops)
(require typed/racket/unsafe)
  
(unsafe-require/typed
 racket/unsafe/ops
 [(unsafe-fx+ unsafe-b+) (-> Index Byte Byte)]
 [(unsafe-fx* unsafe-b*) (-> Index Byte Byte)]
 [(unsafe-fxrshift unsafe-brshift) (case-> [Index Byte -> Byte])]
 
 [(unsafe-fx* unsafe-idx*) (-> Natural Natural Index)]
 [(unsafe-fx+ unsafe-idx+) (-> Natural Natural Index)]
 [(unsafe-fx- unsafe-idx-) (case-> [Byte Byte -> Byte]
                                   [Zero Negative-Fixnum -> Index]
                                   [Natural Natural -> Index])]
 [(unsafe-fxxor unsafe-idxxor) (-> Integer Natural Index)]
 [(unsafe-fxlshift unsafe-idxlshift) (case-> [Positive-Integer Fixnum -> Positive-Index]
                                             [Natural Fixnum -> Index])]
 [(unsafe-fxrshift unsafe-idxrshift) (case-> [Byte Byte -> Byte]
                                             [Natural Byte -> Index]
                                             [Natural Natural -> Index])])
