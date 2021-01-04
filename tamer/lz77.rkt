#lang typed/racket/base

(require "../digitama/bintext/lz77.rkt")
(require "../digitama/bintext/zipconfig.rkt")
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
  (define lz77-test : (->* (Bytes) (Byte Byte) Boolean)
    (lambda [txt [level0 3] [leveln (add1 level0)]]
      (displayln txt)

      (define summaries : (Listof (List Boolean String Byte))
        (for/list ([level (in-range level0 leveln)])
          (with-asserts ([level byte?])
            (define preference : ZIP-Deflation-Config (zip-compression-preference level)) 
            (printf "==> [level: ~a]~n" preference)
          
            (define count : Index (lz77-deflate txt display-codeword preference))
            (define-values (?txt total) (lz77-inflate (in-vector magazine 0 count)))
            
            (newline)
            
            (let ([ok? (bytes=? txt ?txt)])
              (when (not ok?)
                (displayln '==>)
                (displayln ?txt))
              
              (list ok? (~% (- 1.0 (/ count (bytes-length txt)))) level)))))

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
  
  (lz77-test text 0 10)
  (lz77-test #"Fa-la-la-la-la" 0 10))
