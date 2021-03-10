#lang typed/racket/base

(require digimon/digitama/bintext/huffman)
(require digimon/bitstream)
(require digimon/format)
(require digimon/spec)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define t:alphabet : Huffman-Alphabet (huffman-make-alphabet upbits))
(define t:codewords : (Mutable-Vectorof Index) (make-vector uplitcode))
(define t:lengths : Bytes (make-bytes uplitcode))

(define basic-freqs : (Vectorof Index) (vector 8 1 1 2 5 10 9 1 0 0 0 0 0 0 0 0 1 3 5))
(define one-freqs : (Vectorof Index) (vector 0 0 0 4))
(define two-freqs : (Vectorof Index) (vector 1 0 0 4))
(define none-freqs : (Vectorof Index) (vector 0 0 0 0))
(define limited-freqs : (Vectorof Index) (vector 1 2 4 8))
(define crazy-freqs : (Vectorof Index) (vector 16383 16384 16384 16384))
(define full-freqs : (Vectorof Index) (make-vector uplitcode 1))

(define basic-lengths : (Listof Byte) (list 3 3 3 3 3 3 4 4 0 0 0 0 0 0 0 0 6 5 4))
(define extreme-lengths : (Listof Byte) (list 3 3 3 15 15 15))
(define rfc1951-lengths : (Listof Byte) (list 3 3 3 3 3 2 4 4))
(define evil-lengths : (Listof Byte) (list 1 2 3
                                           4 4 4 4 4 4 4 4 4 4
                                           4 4 4 4 4 4 4 4 4 4
                                           4 4 4 4 4 4 4 4 4 4
                                           4 4 4 4 4 4 4 4 4 4
                                           4 4 4 4 4 4 4 4 4 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define huffman-symbol-extract/length : (->* (Huffman-Alphabet Index) (Byte) Byte)
  (lambda [alphabet codeword+more [bwidth (huffman-alphabet-max-bwidth alphabet)]]
    (define-values (symbol length) (huffman-symbol-extract alphabet 0))
    length))

(define huffman-frequencies->length-limited-tree : (-> (Vectorof Index) (Mutable-Vectorof Index) Bytes Byte Void)
  (lambda [freqs codewords lengths length-limit]
    (huffman-frequencies->tree! freqs codewords lengths #:length-limit length-limit)
    (void)))

(define-behavior (it-will-extract-symbol alphabet msb-code symbol-code symbol-bits)
  (let ([lsb-code (bits-reverse-uint16 msb-code symbol-bits)])
    #:it ["will extract symbol '~a' from bit stream '~a' [LSB: 0x~a] [MSB: 0x~a]"
          symbol-code (~binstring lsb-code symbol-bits) (~hexstring lsb-code) (~hexstring msb-code)] #:do
    (let-values ([(symbol length) (huffman-symbol-extract alphabet lsb-code)])
      (when (> symbol-bits 0)
        (expect-= symbol symbol-code))
      (expect-= length symbol-bits))))

(define-behavior (it-check-length ls idx v)
  #:it ["~a[~a] => ~a" 'ls idx v] #:do
  (expect-= (bytes-ref ls idx) v))

(define-behavior (it-check-codeword cds idx v)
  #:it ["~a[~a] => 0x~a" 'cds idx (~hexstring v)] #:do
  (expect-= (vector-ref cds idx) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature huffman #:do
  (describe "canonical huffman codes" #:do
            (context "lengths and codewords" #:do
                     (context ["when provided with frequencies ~a" basic-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree basic-freqs t:codewords t:lengths 6)) #:do
                              (it-check-length t:lengths 0  3)
                              (it-check-length t:lengths 1  6)
                              (it-check-length t:lengths 2  6)
                              (it-check-length t:lengths 3  5)
                              (it-check-length t:lengths 4  3)
                              (it-check-length t:lengths 5  2)
                              (it-check-length t:lengths 6  2)
                              (it-check-length t:lengths 7  6)
                              (it-check-length t:lengths 8  0)
                              (it-check-length t:lengths 9  0)
                              (it-check-length t:lengths 10 0)
                              (it-check-length t:lengths 11 0)
                              (it-check-length t:lengths 12 0)
                              (it-check-length t:lengths 13 0)
                              (it-check-length t:lengths 14 0)
                              (it-check-length t:lengths 15 0)
                              (it-check-length t:lengths 16 6)
                              (it-check-length t:lengths 17 5)
                              (it-check-length t:lengths 18 3)
                              
                              (it-check-codeword t:codewords 5  #x0)
                              (it-check-codeword t:codewords 6  #x2)
                              (it-check-codeword t:codewords 0  #x1)
                              (it-check-codeword t:codewords 4  #x5)
                              (it-check-codeword t:codewords 18 #x3)
                              (it-check-codeword t:codewords 3  #x7)
                              (it-check-codeword t:codewords 17 #x17)
                              (it-check-codeword t:codewords 1  #x0f)
                              (it-check-codeword t:codewords 2  #x2f)
                              (it-check-codeword t:codewords 7  #x1f)
                              (it-check-codeword t:codewords 16 #x3f))

                     (context ["when provided with frequencies ~a for tiny tree" one-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree one-freqs t:codewords t:lengths 6)) #:do
                              (it-check-length t:lengths 0  0)
                              (it-check-length t:lengths 1  0)
                              (it-check-length t:lengths 2  0)
                              (it-check-length t:lengths 3  1))
                     
                     (context ["when provided with frequencies ~a for tiny tree" two-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree two-freqs t:codewords t:lengths 6)) #:do
                              (it-check-length t:lengths 0  1)
                              (it-check-length t:lengths 1  0)
                              (it-check-length t:lengths 2  0)
                              (it-check-length t:lengths 3  1))
                     
                     (context ["when provided with zero frequencies ~a" none-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree none-freqs t:codewords t:lengths 6)) #:do
                              (it-check-length t:lengths 0  0)
                              (it-check-length t:lengths 1  0)
                              (it-check-length t:lengths 2  0)
                              (it-check-length t:lengths 3  0))
                     
                     (context ["when provided with frequencies ~a for limited encoding" limited-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree limited-freqs t:codewords t:lengths 2)) #:do
                              (it-check-length t:lengths 0  2)
                              (it-check-length t:lengths 1  2)
                              (it-check-length t:lengths 2  2)
                              (it-check-length t:lengths 3  2))
                     
                     (context ["when provided with crazy frequencies ~a" crazy-freqs] #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree crazy-freqs t:codewords t:lengths 10)) #:do
                              (it-check-length t:lengths 0  2)
                              (it-check-length t:lengths 1  2)
                              (it-check-length t:lengths 2  2)
                              (it-check-length t:lengths 3  2))
                     
                     (context "when provided with all-one frequencies" #:do
                              #:before (λ [] (huffman-frequencies->length-limited-tree full-freqs t:codewords t:lengths 15)) #:do
                              (it "has a length of 8 or 9 for all symbols" #:do
                                  (for ([idx (in-range uplitcode)])
                                    (expect-member (bytes-ref t:lengths idx) (list 8 9))))))
            (context "alphabet" #:do
                     (context ["when provided with lengths ~a" basic-lengths] #:do
                              #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes basic-lengths))) #:do
                              (it-will-extract-symbol t:alphabet #b000     0      3)
                              (it-will-extract-symbol t:alphabet #b011     3      3)
                              (it-will-extract-symbol t:alphabet #b11110   17     5)
                              (it-will-extract-symbol t:alphabet #b111110  16     6)
                              (it-will-extract-symbol t:alphabet #b1111111 #false 0))
                     
                     (context ["when provided with lengths ~a" rfc1951-lengths] #:do
                              #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes rfc1951-lengths))) #:do
                              (it-will-extract-symbol t:alphabet #b010  0 3)
                              (it-will-extract-symbol t:alphabet #b011  1 3)
                              (it-will-extract-symbol t:alphabet #b100  2 3)
                              (it-will-extract-symbol t:alphabet #b101  3 3)
                              (it-will-extract-symbol t:alphabet #b110  4 3)
                              (it-will-extract-symbol t:alphabet #b00   5 2)
                              (it-will-extract-symbol t:alphabet #b1110 6 4)
                              (it-will-extract-symbol t:alphabet #b1111 7 4))
                     
                     (context ["when provided with extreme lengths ~a" extreme-lengths] #:do
                              #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes extreme-lengths))) #:do
                              (it-will-extract-symbol t:alphabet 0 0 3)
                              (it-will-extract-symbol t:alphabet 1 1 3)
                              (it-will-extract-symbol t:alphabet 2 2 3)
                              
                              (it-will-extract-symbol t:alphabet #x3000 3 15)
                              (it-will-extract-symbol t:alphabet #x3001 4 15)
                              (it-will-extract-symbol t:alphabet #x3002 5 15))
                     
                     (context ["when provided with malicious lengths ~a" evil-lengths] #:do
                              (it "will overflow the sentinel bits" #:do
                                  (expect-throw exn:fail?
                                                (λ [] (huffman-alphabet-canonicalize!
                                                       t:alphabet (apply bytes evil-lengths))))))

                     (context "when provided with an empty lengths" #:do
                              #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet #"")) #:do
                              (it "won't hit any symbol in its cheatsheet" #:do
                                  (expect-zero (huffman-symbol-extract/length t:alphabet 0)))
                              (it "won't hit any symbol in its alphabet" #:do
                                  (expect-zero (huffman-symbol-extract/length t:alphabet #xFFFF)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove huffman)))
