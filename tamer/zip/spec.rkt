#lang typed/racket/base

(require racket/vector)

(require digimon/digitama/bintext/huffman)
(require digimon/digitama/bintext/deflate)
(require digimon/digitama/bintext/zip)
(require digimon/bitstream)
(require digimon/format)
(require digimon/spec)

(require "huftree.rkt")

(require (for-syntax racket/base))

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

(define deflated-twocities : Bytes
  (bytes #x74 #xeb #xcd #x0d #x80 #x20 #x0c #x47 #x71 #xdc #x9d #xa2 #x03 #xb8 #x88 #x63
         #xf0 #xf1 #x47 #x9a #x00 #x35 #xb4 #x86 #xf5 #x0d #x27 #x63 #x82 #xe7 #xdf #x7b
         #x87 #xd1 #x70 #x4a #x96 #x41 #x1e #x6a #x24 #x89 #x8c #x2b #x74 #xdf #xf8 #x95
         #x21 #xfd #x8f #xdc #x89 #x09 #x83 #x35 #x4a #x5d #x49 #x12 #x29 #xac #xb9 #x41
         #xbf #x23 #x2e #x09 #x79 #x06 #x1e #x85 #x91 #xd6 #xc6 #x2d #x74 #xc4 #xfb #xa1
         #x7b #x0f #x52 #x20 #x84 #x61 #x28 #x0c #x63 #xdf #x53 #xf4 #x00 #x1e #xc3 #xa5
         #x97 #x88 #xf4 #xd9 #x04 #xa5 #x2d #x49 #x54 #xbc #xfd #x90 #xa5 #x0c #xae #xbf
         #x3f #x84 #x77 #x88 #x3f #xaf #xc0 #x40 #xd6 #x5b #x14 #x8b #x54 #xf6 #x0f #x9b
         #x49 #xf7 #xbf #xbf #x36 #x54 #x5a #x0d #xe6 #x3e #xf0 #x9e #x29 #xcd #xa1 #x41
         #x05 #x36 #x48 #x74 #x4a #xe9 #x46 #x66 #x2a #x19 #x17 #xf4 #x71 #x8e #xcb #x15
         #x5b #x57 #xe4 #xf3 #xc7 #xe7 #x1e #x9d #x50 #x08 #xc3 #x50 #x18 #xc6 #x2a #x19
         #xa0 #xdd #xc3 #x35 #x82 #x3d #x6a #xb0 #x34 #x92 #x16 #x8b #xdb #x1b #xeb #x7d
         #xbc #xf8 #x16 #xf8 #xc2 #xe1 #xaf #x81 #x7e #x58 #xf4 #x9f #x74 #xf8 #xcd #x39
         #xd3 #xaa #x0f #x26 #x31 #xcc #x8d #x9a #xd2 #x04 #x3e #x51 #xbe #x7e #xbc #xc5
         #x27 #x3d #xa5 #xf3 #x15 #x63 #x94 #x42 #x75 #x53 #x6b #x61 #xc8 #x01 #x13 #x4d
         #x23 #xba #x2a #x2d #x6c #x94 #x65 #xc7 #x4b #x86 #x9b #x25 #x3e #xba #x01 #x10
         #x84 #x81 #x28 #x80 #x55 #x1c #xc0 #xa5 #xaa #x36 #xa6 #x09 #xa8 #xa1 #x85 #xf9
         #x7d #x45 #xbf #x80 #xe4 #xd1 #xbb #xde #xb9 #x5e #xf1 #x23 #x89 #x4b #x00 #xd5
         #x59 #x84 #x85 #xe3 #xd4 #xdc #xb2 #x66 #xe9 #xc1 #x44 #x0b #x1e #x84 #xec #xe6
         #xa1 #xc7 #x42 #x6a #x09 #x6d #x9a #x5e #x70 #xa2 #x36 #x94 #x29 #x2c #x85 #x3f
         #x24 #x39 #xf3 #xae #xc3 #xca #xca #xaf #x2f #xce #x8e #x58 #x91 #x00 #x25 #xb5
         #xb3 #xe9 #xd4 #xda #xef #xfa #x48 #x7b #x3b #xe2 #x63 #x12 #x00 #x00 #x20 #x04
         #x80 #x70 #x36 #x8c #xbd #x04 #x71 #xff #xf6 #x0f #x66 #x38 #xcf #xa1 #x39 #x11
         #x0f))

(define /dev/bsout : Output-Port (open-output-bytes))

(define-syntax (open-lsb-input-bitstream-from-bytes stx)
  (syntax-case stx []
    [(_ bs argl ...)
     (syntax/loc stx
       (make-input-lsb-bitstream (open-input-bytes bs) argl ...))]))

(define-syntax (open-lsb-input-bitstream-from-string-port stx)
  (syntax-case stx []
    [(_ /dev/strin argl ...)
     (syntax/loc stx
       (open-lsb-input-bitstream-from-bytes (get-output-bytes /dev/strin #false) argl ...))]))

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
    #:it ["should extract symbol '~a' from codeword '~a' [LSB: 0x~a] [MSB: 0x~a]"
          symbol-code (~binstring lsb-code symbol-bits) (~hexstring lsb-code) (~hexstring msb-code)] #:do
    (let-values ([(symbol length) (huffman-symbol-extract alphabet lsb-code)])
      (expect-bin= symbol symbol-code)
      (expect-= length symbol-bits))))

(define-behavior (it-will-extract-nothing-from alphabet msb-code symbol-bits)
  (let ([lsb-code (bits-reverse-uint16 msb-code symbol-bits)])
    #:it ["shouldn't extract any symbol from codeword '~a' [LSB: 0x~a] [MSB: 0x~a]"
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
                               (it-will-extract-symbol-from t:alphabet  #b000     0  3)
                               (it-will-extract-symbol-from t:alphabet  #b011     3  3)
                               (it-will-extract-symbol-from t:alphabet  #b11110   17 5)
                               (it-will-extract-symbol-from t:alphabet  #b111110  16 6)
                               (it-will-extract-nothing-from t:alphabet #b1111111    7))
                     
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
                     
                      (context ["when provided with long lengths ~a" extreme-lengths] #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet (apply bytes extreme-lengths))) #:do
                               (it-will-extract-symbol-from t:alphabet 0 0 3)
                               (it-will-extract-symbol-from t:alphabet 1 1 3)
                               (it-will-extract-symbol-from t:alphabet 2 2 3)
                              
                               (it-will-extract-symbol-from t:alphabet #x3000 3 15)
                               (it-will-extract-symbol-from t:alphabet #x3001 4 15)
                               (it-will-extract-symbol-from t:alphabet #x3002 5 15))
                     
                      (context ["when provided with malicious lengths ~a" evil-lengths] #:do
                               (it "should throw an exception due to sentinel bits overflow" #:do
                                   (expect-throw exn:fail?
                                                 (λ [] (huffman-alphabet-canonicalize!
                                                        t:alphabet (apply bytes evil-lengths))))))

                      (context "when provided with an empty lengths" #:do
                               #:before (λ [] (huffman-alphabet-canonicalize! t:alphabet #"")) #:do
                               (it "shouldn't hit any symbol in its cheatsheet" #:do
                                   (expect-zero (huffman-symbol-extract/length t:alphabet 0)))
                               (it "shouldn't hit any symbol in its alphabet" #:do
                                   (expect-zero (huffman-symbol-extract/length t:alphabet #xFFFF))))))

  (describe "lsb input bitstream" #:do
            (let-values ([(push-bits send-bits _) (open-output-lsb-bitstream /dev/bsout)])
              (it "should construct a 2-byte bitstream" #:do
                  (push-bits #b1101  4)
                  (push-bits #b110   3)
                  (push-bits #b1011  4)
                  (push-bits #b10001 5)
                  (expect-= (send-bits #:windup? #true) 2)))
            
            (it "should arrange bits LSB-first but the forming bytes MSB-first" #:do
                (expect-bytes= (get-output-bytes /dev/bsout #false) (bytes #b11101101 #b10001101)))
            
            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-string-port /dev/bsout)])
              (context "with the certain 2-byte bitstream" #:do
                       (it "should be okay to accept feeding request for 16 bits" #:do
                           (expect-true (feed-bits 16)))
                       (it "should read original bits in exactly same order" #:do
                           (expect-bin= (begin0 (peek-bits 4 #b1111)  (fire-bits 4)) #b1101)
                           (expect-bin= (begin0 (peek-bits 3 #b111)   (fire-bits 3)) #b110)
                           (expect-bin= (begin0 (peek-bits 4 #b1111)  (fire-bits 4)) #b1011)
                           (expect-bin= (begin0 (peek-bits 5 #b11111) (fire-bits 5)) #b10001))
                       (it "should report 2 as the number of bytes actually fired" #:do
                           (expect-= ($ 'aggregate) 2))))

            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-string-port /dev/bsout #:lookahead 0 #:limited 1)])
              (context "with the certain 2-byte bitstream, truncated to 1-byte, padded with `#xFF`" #:do
                       (it "should be okay to accept feeding request for 16 bits, but return `#false` to indicate that it has exhausted" #:do
                           (expect-false (feed-bits 16)))
                       (it "should only restore the first half of orginal text" #:do
                           (expect-bin= (begin0 (peek-bits 4 #b1111)  (fire-bits 4)) #b1101)
                           (expect-bin= (begin0 (peek-bits 3 #b111)   (fire-bits 3)) #b110))
                       (it "should employ the padding byte for following feeding request" #:do
                           (expect-bin= (begin0 (peek-bits 4 #b1111)  (fire-bits 4)) #b1111)
                           (expect-bin= (begin0 (peek-bits 5 #b11111) (fire-bits 5)) #b11111))
                       (it "should report 2 as the number of bytes actually fired" #:do
                           (expect-= ($ 'aggregate) 2))))

            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-string-port /dev/bsout #:lookahead 0)])
              (context "with the certain 2-byte bitstream, no lookahead" #:do
                       (it "should be okay to accept feeding request for 255 bits, but return `#false` to indicate that it has exhausted" #:do
                           (expect-false (feed-bits 255)))
                       (it "should skip 5 bits to align to the next byte boundary after firing 3 bits" #:do
                           (fire-bits 3)
                           (expect-bin= ($ 'align) #b11101)
                           (expect-bin= (begin0 (peek-bits 8) (fire-bits 8)) #b10001101))
                       (it "should report 32 as the number of bytes actually fired before and after committing" #:do
                           (expect-= ($ 'aggregate) 32)
                           ($ 'final-commit)
                           (expect-= ($ 'aggregate) 32))))

            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-string-port /dev/bsout #:lookahead 4)])
              (context "with the certain 2-byte bitstream, 4-byte lookahead, padded with `#xFF`" #:do
                       (it "should employ the padding byte after exhausting" #:do
                           (feed-bits 255)
                           (fire-bits 16)
                           (expect-hex= (peek-bits 32) #xFFFFFFFF))
                       (it "should report 28 as the number of bytes actually fired before and after committing" #:do
                           (expect-= ($ 'aggregate) 28)
                           ($ 'final-commit)
                           (expect-= ($ 'aggregate) 28))))

            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-string-port /dev/bsout #:lookahead 4)])
              (context "with the certain 2-byte bitstream, 4-byte lookahead, padded with `#xFF`, fired 240 bits" #:do
                       (it "should report 30 as the number of bytes actually fired before and after committing" #:do
                           (feed-bits 255)
                           (fire-bits 240)
                           (expect-= ($ 'aggregate) 30)
                           ($ 'final-commit)
                           (expect-= ($ 'aggregate) 30))))
            
            (let*-values ([(n) 30000]
                          [(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-bytes (make-bytes n))])
              (context ["with a ~a-byte bitstream" n] #:do
                       (it ["should report ~a as the number of bytes actually fired after exhausting it before and after committing" n] #:do
                           (let exhaust ()
                             (when (feed-bits 16)
                               (peek-bits) (fire-bits 16)
                               (exhaust)))
                           (expect-= ($ 'aggregate) n)
                           ($ 'final-commit)
                           (expect-= ($ 'aggregate) n)))))

  (describe "inflate" #:do
            (context "when provided with an invalid block" #:do
                     (it "should throw an exception due to unkown block type" #:do
                         (expect-throw "unknown deflate block type"
                                       (λ [] (inflate (bytes #x06))))))

            (context "when provided with an stored block" #:do
                     (it "should return EOF immediately due to empty block" #:do
                         (expect-eof (open-input-deflated-block (open-input-bytes bad-stored-block) 0 #:name "/dev/zero")))
                     (it "should throw an exception due to broken length fields" #:do
                         (expect-throw "unexpected end of file"
                                       (λ [] (inflate bad-stored-block 4))))
                     (it "should throw an exception due to invalid block length" #:do
                         (expect-throw "invalid block length"
                                       (λ [] (inflate bad-stored-block))))
                     (it "should throw an exception due to unexpected end of file" #:do
                         (expect-throw "unexpected end of file"
                                       (λ [] (inflate good-stored-block 9))))
                     (it "should read the uncompressed text" #:do
                         (expect-bytes= (inflate good-stored-block) #"Hello")))

            (let-values ([(feed-bits peek-bits fire-bits $) (open-lsb-input-bitstream-from-bytes deflated-twocities)]
                         [(hlit hdist hclen) (values 0 0 0)]
                         [(codelen-lengths) (make-bytes uplencode 0)]
                         [(length-codes) ((inst make-vector (U (Pairof Byte Byte) Byte)) (+ uplitcode updistcode) 0)]
                         [(codeword-lengths) (make-bytes (+ uplitcode updistcode) 0)]
                         [(codelen-alphabet) (huffman-make-alphabet uplencode #:max-bitwidth uplenbits)])
              (context "when provided with a dynamically deflated block" #:do
                       (it "should extract the header of dynamic block" #:do
                           (feed-bits 3)
                           (expect-bin= (peek-bits 2 #b11 1) #b10)
                           (fire-bits 3))
                       
                       (it "should extract the header of dynamic huffman encoding" #:do
                           (feed-bits 14)
                           (expect-= ($! hlit (assert (+ (peek-bits 5 #b11111) literal-nbase) index?)) 271)
                           (expect-= ($! hdist (+ (peek-bits 5 #b11111 5) distance-nbase)) 12)
                           (expect-= ($! hclen (+ (peek-bits 4 #b1111 10) codelen-nbase)) 19)
                           (fire-bits 14))

                       (it ["should extract ~a codelen codes from next ~a bits" hclen (* hclen 3)] #:do
                           (for ([idx (in-range hclen)])
                             (feed-bits 3)
                             (bytes-set! codelen-lengths (bytes-ref codelen-lengths-order idx) (peek-bits 3 #b111))
                             (fire-bits 3))
                           (expect-bytes= codelen-lengths
                                          (bytes 3 7 5 7 3 2 2 0 0 0 0 0 0 0 0 0 6 4 3)))
                       
                       (it "should be inflated and restored to its original text" #:do
                           #:timeout/ms 200 #:do
                           (expect-bytes= (inflate deflated-twocities) #"Hello"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove huffman))

  #;(displayln (bytes->bin-string deflated-twocities #:separator " ")))
