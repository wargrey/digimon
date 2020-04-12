#lang typed/racket

(require digimon/plist)

(require math/base)

(define plist : PList-Datum
  (vector (vector (void) #true #false)
          (vector euler.0  (- phi.0))
          (vector (- #x1234) #x567 #x89ABCDEF #x00)))
