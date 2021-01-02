#lang typed/racket/base

(require "../digitama/bintext/lz77.rkt")
(require "../digitama/unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define magazine : (Vectorof (U Byte (Pairof Index Index) EOF)) (make-vector (expt 2 15) eof))

(define display-codeword : LZ77-Select-Codeword
  (case-lambda
    [(bs src-idx codeword dest-idx)
     (vector-set! magazine dest-idx codeword)
     (display (integer->char codeword))]
    [(bs src-idx pointer size dest-idx)
     (vector-set! magazine dest-idx (cons pointer size))
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
  
  (lz77-deflate text display-codeword #:min-match 4)
  (newline)
  (lz77-deflate #"Fa-la-la-la-la" display-codeword #:min-match 4)
  (newline))
