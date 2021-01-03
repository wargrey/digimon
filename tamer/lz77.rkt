#lang typed/racket/base

(require "../digitama/bintext/lz77.rkt")
(require "../digitama/unsafe/ops.rkt")

(require "../number.rkt")
(require "../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define magazine : (Vectorof (U Byte (Pairof Index Index))) (make-vector (expt 2 15) 0))

(define display-codeword : LZ77-Select-Codeword
  (case-lambda
    [(bs src-idx codeword dest-idx)
     (vector-set! magazine dest-idx codeword)
     (display (integer->char codeword))
     (flush-output (current-output-port))]
    [(bs src-idx pointer size dest-idx)
     (vector-set! magazine dest-idx (cons pointer size))
     (display (cons pointer size))
     (flush-output (current-output-port))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define lz77-test : (->* (Bytes) (Positive-Byte Positive-Byte) Boolean)
    (lambda [txt [match0 3] [matchn (add1 match0)]]
      (displayln txt)

      (define summaries : (Listof (List Boolean String Positive-Byte))
        (for/list ([min-match (in-range match0 matchn)])
          (printf "==> [min-match: ~a]~n" min-match)
          
          (with-asserts ([min-match positive-byte?])
            (define count : Index (lz77-deflate txt display-codeword #:min-match min-match))
            (define-values (?txt total) (lz77-inflate (in-vector magazine 0 count)))
            
            (newline)
            
            (let ([ok? (bytes=? txt ?txt)])
              (when (not ok?)
                (displayln '==>)
                (displayln ?txt))
              
              (list ok? (~% (- 1.0 (/ count (bytes-length txt)))) min-match)))))

      (printf "=============~n")
      (for/fold ([ok? : Boolean #true])
                ([summary (in-list summaries)])
        (displayln summary)
        (and ok? (car summary)))))
  
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
  
  (lz77-test text 1 16)
  (lz77-test #"Fa-la-la-la-la" 1 16))
