#lang typed/racket

(require digimon/plist)

(require math/base)

(define plist : PList-Object
  (list (list (void) #true #false)
        (list euler.0  (- phi.0))
        (list (- #x1234) #x567 #x89ABCDEF #x00)))
