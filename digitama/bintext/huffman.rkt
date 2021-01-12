#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/grub/grub-0.97/stage2/gunzip.c
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/list))

(require "../unsafe/ops.rkt")

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

(define-syntax (define-huffman-fixed-distance-table stx)
  (syntax-case stx []
    [(_  huffman-distance-extra-bits huffman-distance-copy-offsets #:with distance->huffman-dist
         [#:tables [huffman-dist base-distance extra-bits] ...])
     (with-syntax ([([hi-distance ...] [lo-distance ...])
                    (let ([bases (reverse (syntax->datum #'(base-distance ...)))]
                          [huffmans (reverse (syntax->datum #'(huffman-dist ...)))])
                      (for/fold ([hi.lo (list (make-list 256 0) (make-list 256 0))])
                                ([dist-1 (in-range 32768)])
                        (let search ([bs bases]
                                     [hs huffmans])
                          (cond [(< (add1 dist-1) (car bs)) (search (cdr bs) (cdr hs))]
                                [(< dist-1 256) (list (car hi.lo) (list-set (cadr hi.lo) dist-1 (car hs)))]
                                [else (list (list-set (car hi.lo) (arithmetic-shift dist-1 -7) (car hs)) (cadr hi.lo))]))))])
       (syntax/loc stx
         (begin (define huffman-distance-extra-bits : (Immutable-Vectorof Byte) (vector-immutable extra-bits ...))
                (define huffman-distance-copy-offsets : (Immutable-Vectorof Index) (vector-immutable base-distance ...))

                (define hi-distances : (Immutable-Vectorof Byte) (vector-immutable hi-distance ...))
                (define lo-distances : (Immutable-Vectorof Byte) (vector-immutable lo-distance ...))
                
                (define distance->huffman-dist : (-> Index Byte)
                  (lambda [distance]
                    (if (<= distance 256)
                        (unsafe-vector*-ref lo-distances (unsafe-idx- distance 1))
                        (unsafe-vector*-ref hi-distances (unsafe-idxrshift (unsafe-idx- distance 1) 7))))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
; The inflate algorithm uses a sliding 32K byte window on the uncompressed stream to find repeated byte strings.
; This is implemented here as a circular buffer.  The index is updated simply by incrementing and then `and`ing with 0x7fff (32K-1).

(define window-bits : Positive-Byte 15)      ; bits of the window size that at least 32K  
(define literal-bits : Positive-Byte 9)      ; bits in base literal/length lookup table
(define distance-bits : Positive-Byte 6)     ; bits in base distance lookup table

(define upbits : Positive-Byte 16)           ; maximum bit length of any code (16 for explode)
(define upcodewords : Positive-Index 288)    ; maximum number of codes in any set, bytes + backreferences + EOB
(define EOB : Index #x100)                   ; end of (huffman) block

(define bit-order : (Immutable-Vectorof Byte) ; Order of the bit length code lengths
  (vector-immutable 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;; NOTE:
; The length code 284 can represent 227-258, but length code 285 really is 258.
; The last length deserves its own, short code since it gets used a lot in very redundant files.
; The length 258 is special since 258 = 255 + 3 (the min match length).
(define huffman-literal-copy-bits : (Immutable-Vectorof Index) ; for codes in range [257, 285]
  (vector-immutable 3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
                    35 43 51 59 67 83 99 115 131 163 195 227 258 0 0))

(define huffman-literal-extra-bits : (Immutable-Vectorof Byte)
  (vector-immutable 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2
                    3 3 3 3 4 4 4 4 5 5 5 5 0 99 99)) ; /* 99 => invalid */

(define-huffman-fixed-distance-table
  huffman-distance-extra-bits
  huffman-distance-copy-offsets
  #:with distance->huffman-dist
  [#:tables
   [0  1      0]
   [1  2      0]
   [2  3      0]
   [3  4      0]
   [4  5      1]
   [5  7      1]
   [6  9      2]
   [7  13     2]
   [8  17     3]
   [9  25     3]
   [10 33     4]
   [11 49     4]
   [12 65     5]
   [13 97     5]
   [14 129    6]
   [15 193    6]
   [16 257    7]
   [17 385    7]
   [18 513    8]
   [19 769    8]
   [20 1025   9]
   [21 1537   9]
   [22 2049  10]
   [23 3073  10]
   [24 4097  11]
   [25 6145  11]
   [26 8193  12]
   [27 12289 12]
   [28 16385 13]
   [29 24577 13]])

(define mask_bits : (Immutable-Vectorof Index)
  (vector-immutable #x0000
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
