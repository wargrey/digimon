#lang typed/racket/base

(provide (all-defined-out))

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/grub/grub-0.97/stage2/gunzip.c
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (huffman-fixed-literal-codeword-bit-lengths stx)
  (syntax-case stx []
    [(_ ) ; NOTE: indices in [286, 287] are unused
     (with-syntax ([(lengths ...)
                    (for/list ([idx (in-range 288)])
                      (cond [(<= idx 143) 8] ; [#b00110000,  #b10111111]
                            [(<= idx 255) 9] ; [#b110010000, #b111111111]
                            [(<= idx 279) 7] ; [#b0000000,   #b0010111]
                            [else 8]))])     ; [#b11000000,  #b11000111], the value is actually useless
       (syntax/loc stx
         (vector-immutable lengths ...)))]))

(define-syntax (huffman-fixed-distance-codeword-bit-lengths stx)
  (syntax-case stx []
    [(_ ) ; NOTE: indices in [30, 31] are unused
     (with-syntax ([(lengths ...) (for/list ([idx (in-range 32)]) 5)])
       (syntax/loc stx
         (vector-immutable lengths ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
; The inflate algorithm uses a sliding 32K byte window on the uncompressed stream to find repeated byte strings.
; This is implemented here as a circular buffer.  The index is updated simply by incrementing and then `and`ing with 0x7fff (32K-1).

(define window-size : Index #x8000) ; must be at least 32K and a power of 2  
(define literal-bits : Byte 9)      ; bits in base literal/length lookup table
(define distance-bits : Byte 6)     ; bits in base distance lookup table

(define upbits : Byte 16)           ; maximum bit length of any code (16 for explode)
(define upcodewords : Index 288)    ; maximum number of codes in any set
(define EOB : Index #x100)          ; end of (huffman) block

(define bit-order : (Vectorof Byte) ; Order of the bit length code lengths
  (vector 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;; NOTE:
; The length code 284 can represent 227-258, but length code 285 really is 258.
; The last length deserves its own, short code since it gets used a lot in very redundant files.
; The length 258 is special since 258 = 255 + 3 (the min match length).
(define huffman-literal-copy-bits : (Vectorof Index) ; for codes in range [257, 285]
  (vector 3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
          35 43 51 59 67 83 99 115 131 163 195 227 258 0 0))

(define huffman-literal-extra-bits : (Vectorof Byte)
  (vector 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2
          3 3 3 3 4 4 4 4 5 5 5 5 0 99 99)) ; /* 99 ==> invalid */

(define huffman-distance-copy-offsets : (Vectorof Index) ; for codes in range [0, 29]
  (vector 1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
          257 385 513 769 1025 1537 2049 3073 4097 6145
          8193 12289 16385 24577))

(define huffman-distance-extra-bits : (Vectorof Byte)
  (vector 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6
          7 7 8 8 9 9 10 10 11 11
          12 12 13 13))

(define mask_bits : (Vectorof Index)
  (vector #x0000
          #x0001 #x0003 #x0007 #x000f #x001f #x003f #x007f #x00ff
          #x01ff #x03ff #x07ff #x0fff #x1fff #x3fff #x7fff #xffff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hufman-fixed-literal-lengths : (Immutable-Vectorof Byte) (huffman-fixed-literal-codeword-bit-lengths))
(define hufman-fixed-distance-lengths : (Immutable-Vectorof Byte) (huffman-fixed-distance-codeword-bit-lengths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct huffman-lookup-table
  ([extra : Byte]
   [base : Byte]
   [secondary : (U Byte Huffman-Lookup-Table)])
  #:type-name Huffman-Lookup-Table
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-huffman-symbols : (->* (Bytes) (Natural Natural) Bytes)
  (lambda [src [start 0] [end (bytes-length src)]]
    (define counts : (Vectorof Natural) (make-vector #xFF 0))
    (define symbols : Bytes (make-bytes (- end start)))

    (for ([b (in-bytes src start end)])
      (vector-set! counts b (add1 (vector-ref counts b))))

    (for ([idx (in-range 1 256)])
      (vector-set! counts idx
                   (+ (vector-ref counts idx)
                      (vector-ref counts (sub1 idx)))))

    (for ([b (in-bytes src start end)])
      (bytes-set! symbols b (add1 (vector-ref counts b)))
      (vector-set! counts b (add1 (vector-ref counts b))))

    symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-huffman-lookup-table : (-> (Immutable-Vectorof Byte) Index Index (Vectorof Index) (Vectorof Byte)
                                        (Values (Option Huffman-Lookup-Table) Index Boolean))
  (lambda [bits total simple-count bases extras]
    (values #false 0 #true)))
