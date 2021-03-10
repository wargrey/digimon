#lang typed/racket/base

(require digimon/digitama/bintext/huffman)
(require digimon/digitama/bintext/deflate)
(require digimon/digitama/bintext/zip)
(require digimon/bitstream)
(require digimon/format)
(require digimon/spec)

(require "huftree.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define t:alphabet : Huffman-Alphabet (huffman-make-alphabet uplitcode))
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

(define bad-stored-block : Bytes (bytes #x01 #x05 #x00 #x12 #x34))
(define good-stored-block : Bytes (bytes-append (bytes #x01 #x05 #x00 #xfa #xff) (string->bytes/utf-8 "Hello")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define huffman-symbol-extract/length : (->* (Huffman-Alphabet Index) (Byte) Byte)
  (lambda [alphabet codeword+more [bwidth (huffman-alphabet-max-bwidth alphabet)]]
    (define-values (symbol length) (huffman-symbol-extract alphabet 0))
    length))

(define huffman-frequencies->length-limited-tree : (-> (Vectorof Index) (Mutable-Vectorof Index) Bytes Byte Void)
  (lambda [freqs codewords lengths length-limit]
    (huffman-frequencies->tree! freqs codewords lengths #:length-limit length-limit)
    (void)))

(define inflate : (->* (Bytes) (Index) Bytes)
  (lambda [src [csize (bytes-length src)]]
    (define /dev/bsout (open-output-bytes '/dev/bsout))
    
    (let ([/dev/zipin (open-input-deflated-block (open-input-bytes src) csize #:name "/dev/zipin")])
      (zip-entry-copy /dev/zipin /dev/bsout))

    (get-output-bytes /dev/bsout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-will-extract-symbol-from alphabet msb-code symbol-code symbol-bits)
  (let ([lsb-code (bits-reverse-uint16 msb-code symbol-bits)])
    #:it ["should extract symbol '~a' from bit stream '~a' [LSB: 0x~a] [MSB: 0x~a]"
          symbol-code (~binstring lsb-code symbol-bits) (~hexstring lsb-code) (~hexstring msb-code)] #:do
    (let-values ([(symbol length) (huffman-symbol-extract alphabet lsb-code)])
      (expect-bin= symbol symbol-code)
      (expect-= length symbol-bits))))

(define-behavior (it-will-extract-nothing-from alphabet msb-code symbol-bits)
  (let ([lsb-code (bits-reverse-uint16 msb-code symbol-bits)])
    #:it ["shouldn't extract any symbol from bit stream '~a' [LSB: 0x~a] [MSB: 0x~a]"
          (~binstring lsb-code symbol-bits) (~hexstring lsb-code) (~hexstring msb-code)] #:do
    (let-values ([(symbol length) (huffman-symbol-extract alphabet lsb-code)])
      (expect-= length 0))))

(define-behavior (it-check-length ls idx v)
  #:it ["~a[~a] => ~a" 'ls idx v] #:do
  (expect-= (bytes-ref ls idx) v))

(define-behavior (it-check-codeword cds idx v)
  #:it ["~a[~a] => 0x~a" 'cds idx (~hexstring v)] #:do
  (expect-bin= (vector-ref cds idx) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature huffman #:do
  (describe "canonical huffman codes" #:do
            (describe "lengths and codewords" #:do
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
                               (it "should sum to UINT16-MAX" #:do
                                   (expect-hex= (for/sum ([f (in-vector crazy-freqs)]) f) #xFFFF))
                               (it-check-length t:lengths 0  2)
                               (it-check-length t:lengths 1  2)
                               (it-check-length t:lengths 2  2)
                               (it-check-length t:lengths 3  2))
                     
                      (context "when provided with all-one frequencies" #:do
                               #:before (λ [] (huffman-frequencies->length-limited-tree full-freqs t:codewords t:lengths 15)) #:do
                               (it "should have lengths of 8 or 9 for all symbols" #:do
                                   (for ([idx (in-range uplitcode)])
                                     (expect-member (bytes-ref t:lengths idx) (list 8 9))))))
            (describe "alphabet" #:do
                      (context ["when provided with lengths ~a" basic-lengths] #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes basic-lengths))) #:do
                               (it-will-extract-symbol-from t:alphabet  #b000     0      3)
                               (it-will-extract-symbol-from t:alphabet  #b011     3      3)
                               (it-will-extract-symbol-from t:alphabet  #b11110   17     5)
                               (it-will-extract-symbol-from t:alphabet  #b111110  16     6)
                               (it-will-extract-nothing-from t:alphabet #b1111111        7))
                     
                      (context ["when provided with lengths ~a" rfc1951-lengths] #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes rfc1951-lengths))) #:do
                               (it-will-extract-symbol-from t:alphabet #b010  0 3)
                               (it-will-extract-symbol-from t:alphabet #b011  1 3)
                               (it-will-extract-symbol-from t:alphabet #b100  2 3)
                               (it-will-extract-symbol-from t:alphabet #b101  3 3)
                               (it-will-extract-symbol-from t:alphabet #b110  4 3)
                               (it-will-extract-symbol-from t:alphabet #b00   5 2)
                               (it-will-extract-symbol-from t:alphabet #b1110 6 4)
                               (it-will-extract-symbol-from t:alphabet #b1111 7 4))
                     
                      (context ["when provided with extreme lengths ~a" extreme-lengths] #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes extreme-lengths))) #:do
                               (it-will-extract-symbol-from t:alphabet 0 0 3)
                               (it-will-extract-symbol-from t:alphabet 1 1 3)
                               (it-will-extract-symbol-from t:alphabet 2 2 3)
                              
                               (it-will-extract-symbol-from t:alphabet #x3000 3 15)
                               (it-will-extract-symbol-from t:alphabet #x3001 4 15)
                               (it-will-extract-symbol-from t:alphabet #x3002 5 15))
                     
                      (context ["when provided with malicious lengths ~a" evil-lengths] #:do
                               (it "should overflow the sentinel bits" #:do
                                   (expect-throw exn:fail?
                                                 (λ [] (huffman-alphabet-canonicalize!
                                                        t:alphabet (apply bytes evil-lengths))))))

                      (context "when provided with an empty lengths" #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet #"")) #:do
                               (it "shouldn't hit any symbol in its cheatsheet" #:do
                                   (expect-zero (huffman-symbol-extract/length t:alphabet 0)))
                               (it "shouldn't hit any symbol in its alphabet" #:do
                                   (expect-zero (huffman-symbol-extract/length t:alphabet #xFFFF))))))

  (describe "inflate" #:do
            (context "when provided with an invalid block" #:do
                     (it "should throw an exception due to unkown block type" #:do
                         (expect-throw "unknown deflate block type"
                                       (λ [] (inflate (bytes #x06))))))
            (context "when provided with an stored block" #:do
                     (it "should return EOF because of an empty block" #:do
                         (expect-eof (read (open-input-deflated-block (open-input-bytes bad-stored-block) 0 #:name "/dev/zero"))))
                     (it "should throw an exception due to broken length fields" #:do
                         (expect-throw "unexpected end of file"
                                       (λ [] (inflate bad-stored-block 4))))
                     (it "should throw an exception due to invalid block length" #:do
                         (expect-throw "invalid block length"
                                       (λ [] (inflate bad-stored-block))))
                     (it "should throw an exception due to unexpected EOF" #:do
                         (expect-throw "unexpected end of file"
                                       (λ [] (inflate good-stored-block 9))))
                     (it "should read the uncompressed text" #:do
                         (expect-bytes (inflate good-stored-block) #"Hello")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove huffman)))
