#lang typed/racket/base

(provide (all-defined-out))
(provide unsafe-bytes-ref unsafe-bytes-set!)
(provide unsafe-fxand unsafe-fxremainder)
(provide (rename-out [unsafe-fx+ unsafe-idx+]
                     [unsafe-fx- unsafe-idx-]
                     [unsafe-fxlshift unsafe-idxlshift]
                     [unsafe-fxrshift unsafe-idxrshift]))


(require racket/unsafe/ops)
(require typed/racket/unsafe)

(unsafe-require/typed
 racket/unsafe/ops
 [unsafe-fx+ (-> Nonnegative-Fixnum Nonnegative-Fixnum Index)]
 [unsafe-fx- (case-> [Byte Byte -> Byte]
                     [Nonnegative-Fixnum Nonnegative-Fixnum -> Index])]
 [unsafe-fxlshift (-> Index Fixnum Index)]
 [unsafe-fxrshift (-> Index Byte Index)])
