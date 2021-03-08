#lang typed/racket/base

(require digimon/echo)

(require digimon/digitama/bintext/huffman)

(require "codelen-cases.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define codelen-symbols : Bytes (make-bytes (+ uplitcode updistcode)))
(define codelen-repeats : Bytes (make-bytes (+ uplitcode updistcode)))
(define codelen-frequencies : (Mutable-Vectorof Index) (make-vector uplencode))

(define codelen-repetition : (-> Byte Index Index Byte)
  (lambda [n idx N]
    (when (> (+ idx n) N)
      (error 'codelen-repetition "repeated codes overflow: ~a (~a left)" n (- N idx)))
    n))

(define codelen-inflate : (-> Index (Listof (U Byte (Pairof Byte Byte))) Bytes)
  (lambda [N codelen-symbols]
    (define codeword-lengths : Bytes (make-bytes N 0))

    (let inflate ([code-idx : Nonnegative-Fixnum 0]
                  [prev-len : Index 0]
                  [symbols : (Listof (U Byte (Pairof Byte Byte))) codelen-symbols])
      (when (and (< code-idx N) (pair? symbols))
        (define-values (len count)
          (let ([s (car symbols)])
            (cond [(pair? s) (values (car s) (cdr s))]
                  [else (values s s)])))
        
        (cond [(< len codelen-copy:2) (bytes-set! codeword-lengths code-idx len) (inflate (+ code-idx 1) len (cdr symbols))]
              [(= len codelen-copy:3) (inflate (+ code-idx (codelen-repetition count code-idx N)) 0 (cdr symbols))]
              [(= len codelen-copy:7) (inflate (+ code-idx (codelen-repetition count code-idx N)) 0 (cdr symbols))]
              [else ; codelen-copy:2) ; to repeat previous length 3 - 6 times, determined by next 2 bits
               (let ([n (codelen-repetition count code-idx N)])
                 (cond [(= prev-len 0) (error 'codelen-inflate "previous length shouldn't be zero")]
                       [else (let ([idx++ (+ code-idx n)])
                               (let copy-code ([idx : Natural code-idx])
                                 (cond [(< idx idx++) (bytes-set! codeword-lengths idx prev-len) (copy-code (+ idx 1))]
                                       [else (inflate idx++ prev-len (cdr symbols))])))]))])))

    codeword-lengths))

(define codelen-test : (-> Bytes (Listof (U Byte (Pairof Byte Byte))) Integer Void)
  (lambda [lengths expected id]
    (huffman-dynamic-lengths-deflate! lengths (bytes-length lengths) codelen-symbols codelen-repeats codelen-frequencies)

    (define given : (Listof (U Byte (Pairof Byte Byte)))
      (for/list : (Listof (U Byte (Pairof Byte Byte))) ([s (in-bytes codelen-symbols)]
                                                        [r (in-bytes codelen-repeats)])
        (cond [(< s codelen-copy:2) s]
              [else (cons s r)])))

    (define okay? : Boolean
      (with-handlers ([exn? (λ _ #false)])
        (bytes=? lengths (codelen-inflate (bytes-length lengths) given))))

    (if (not okay?)
        (echof "~a: Not Okay" id #:fgcolor 'red)
        (echof "~a: Okay" id #:fgcolor 'green))

    (newline)))

(module+ main
  (for/and : Boolean ([testcase (in-list codelen-testcases)])
    (bytes=? (car testcase)
             (codelen-inflate (bytes-length (car testcase))
                              (cdr testcase))))
  
  (for ([testcase (in-list codelen-testcases)]
        [idx (in-naturals 1)])
    (codelen-test (car testcase) (cdr testcase) idx)))
