#lang typed/racket/base

(require "../digitama/bintext/lz77.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define display-codeword : (LZ77-Select-Codeword Void)
  (case-lambda
    [(bs idx codeword _)
     (display (integer->char codeword))]
    [(bs idx pointer size _)
     (display (cons pointer size))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define text : Bytes
    #"It was the best of times,
it was the worst of times,
it was the age of wisdom,
it was the age of foolishness,
it was the epoch of belief,
it was the epoch of incredulity,
it was the season of Light,
it was the season of Darkness,
it was the spring of hope,
it was the winter of despair,
we had everything before us,
we had nothing before us,
we were all going direct to Heaven,
we were all going direct the other way")
  
  (lz77-compress text display-codeword (void))
  (lz77-compress #"Fa-la-la-la-la" display-codeword (void)))
