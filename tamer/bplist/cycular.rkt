#lang typed/racket

(require digimon/plist)

(require typed/racket/random)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rsa : PList-Datum
  (make-hasheq (list (cons 'e 17)
                     (cons 'n 65535))))

(define plist : PList-Datum
  (make-hasheq (list (cons 'rsa1 rsa)
                     (cons 'rsa2 rsa)
                     (cons 'p (crypto-random-bytes 16))
                     (cons 'q (crypto-random-bytes 16)))))
