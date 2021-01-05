#lang typed/racket/base

(require "../digitama/bintext/lz77.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define magazine : (Vectorof (U Byte (Pairof Index Index))) (make-vector (expt 2 15) 0))

(define display-codeword : LZ77-Select-Codeword
  (case-lambda
    [(codeword dest-idx)
     (vector-set! magazine dest-idx codeword)
     (display (integer->char codeword))
     (flush-output (current-output-port))]
    [(pointer size dest-idx)
     (vector-set! magazine dest-idx (cons pointer size))
     (display (cons pointer size))
     (flush-output (current-output-port))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require racket/list)
  
  (require "../format.rkt")
  
  (require "../digitama/bintext/zipconfig.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define lz77-test : (-> Bytes (U ZIP-Strategy (Listof ZIP-Strategy)) Boolean)
    (lambda [txt strategies]
      (displayln txt)

      (define summaries : (Listof (List Boolean String Integer))
        (for/list ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))]
                   [idx (in-naturals 1)])
          (printf "==> [~a][strategy: ~a]~n" idx strategy)
          
          (define count : Index (lz77-deflate txt display-codeword strategy))
          (define-values (?txt total) (lz77-inflate (in-vector magazine 0 count)))
            
          (newline)
          
          (let ([ok? (bytes=? txt ?txt)])
            (when (not ok?)
              (displayln '==>)
              (displayln ?txt))
            
            (list ok? (~% (- 1.0 (/ count (bytes-length txt)))) idx))))

      (printf "=============~n")
      (for/fold ([ok? : Boolean #true])
                ([summary (in-list summaries)])
        (displayln summary)
        (and ok? (car summary)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  (define fastest-strategy : ZIP-Strategy (zip-fastest-preference))
  (define default-strategies : (Listof ZIP-Strategy) (map zip-default-preference (range 0 10)))
  (define rle-strategies : (Listof ZIP-Strategy) (map zip-run-preference (range 1 5)))
  
  (lz77-test text default-strategies)
  (lz77-test text fastest-strategy)
  (lz77-test #"Fa-la-la-la-la" rle-strategies))
