#lang typed/racket/base

;;; TAOCP Vol. 3 P145
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

(define-syntax (define-huffman-fixed-span-literal-table stx)
  (syntax-case stx []
    [(_  huffman-span-literal-extra-bits huffman-span-literal-copy-offsets #:with span->codeword
         [#:tables [literal base-span extra-bits] ...])
     (with-syntax ([(codeword ...)
                    (let* ([bases (reverse (syntax->datum #'(base-span ...)))]
                           [literals (reverse (syntax->datum #'(literal ...)))]
                           [max-span+1 (add1 (car bases))])
                      (for/fold ([codewords (make-list max-span+1 0)])
                                ([span (in-range 3 max-span+1)])
                        (let search ([bs bases]
                                     [hs literals])
                          (cond [(< span (car bs)) (search (cdr bs) (cdr hs))]
                                [else (list-set codewords span (car hs))]))))])
       (syntax/loc stx
         (begin (define huffman-span-literal-extra-bits : (Immutable-Vectorof Byte) (vector-immutable extra-bits ...))
                (define huffman-span-literal-copy-offsets : (Immutable-Vectorof Index) (vector-immutable base-span ...))

                (define codewords : (Immutable-Vectorof Index) (vector-immutable codeword ...))
                
                (define span->codeword : (-> Index Index)
                  (lambda [span]
                    (unsafe-vector*-ref codewords span))))))]))

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
(define upcodes : Positive-Index 288)        ; maximum number of codes in any set, bytes + backreferences + EOB
(define EOB : Index #;256 #x100)             ; end of (huffman) block

(define bit-order : (Immutable-Vectorof Byte) ; Order of the bit length code lengths
  (vector-immutable 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(define-huffman-fixed-span-literal-table
  huffman-literal-extra-bits
  huffman-literal-copy-bits
  #:with backref-span->huffman-symbol
  [#:tables
   [257 3   0]
   [258 4   0]
   [259 5   0]
   [260 6   0]
   [261 7   0]
   [262 8   0]
   [263 9   0]
   [264 10  0]
   [265 11  1]
   [266 13  1]
   [267 15  1]
   [268 17  1]
   [269 19  2]
   [270 23  2]
   [271 27  2]
   [272 31  2]
   [273 35  3]
   [274 43  3]
   [275 51  3]
   [276 59  3]
   [277 67  4]
   [278 83  4]
   [279 99  4]
   [280 115 4]
   [281 131 5]
   [282 163 5]
   [283 195 5]
   [284 227 5]

   ;; NOTE
   ; The 258 can be represented by 284, but it still deserves its own for shorter code
   ; since it gets used a lot in very redundant files, any contents longer will be truncated.
   [285 258 0]])

(define-huffman-fixed-distance-table
  huffman-distance-extra-bits
  huffman-distance-copy-offsets
  #:with backref-distance->huffman-dist
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
;;; The first half of the vector stores the heap pointers,
;;; and the second half of the vector stores the frequencies.
;;; WARNING: the heap starts from 0

(define-syntax (huffman-minheap-key stx)
  (syntax-case stx []
    [(_ fs code)
     (syntax/loc stx
       (unsafe-vector*-ref fs code))]))

(define huffman-refresh-minheap! : (-> (Mutable-Vectorof Index) (Mutable-Vectorof Index) Index Index Index)
  (lambda [freqs heap link-idx0 freq-idx0]
    (define fcount : Index (vector-length freqs))
    (define n : Index
      (let heap-copy! ([i : Nonnegative-Fixnum 0]
                       [n : Index 0])
        (if (>= i fcount) n
            (let ([freq (vector-ref freqs i)]
                  [i++ (add1 i)])
              (if (= freq 0)
                  (heap-copy! i++ n)
                  (let ([f-idx (unsafe-idx+ freq-idx0 n)])
                    (vector-set! heap f-idx freq)
                    (vector-set! heap (+ link-idx0 n) f-idx)
                    (heap-copy! i++ (unsafe-idx+ n 1))))))))

    (huffman-minheapify! heap n)
    n))

; the name "siftup" is a little confusing,
;   it moves up the smallest code which is originally located at the bottom, but
;   the process of sifting is actually known as the top-down approach, and
;   might be given name by someone (say, wikipedia) with "siftDown", in which case
;   the "down" should be replaced by "downwards".  
(define huffman-minheap-siftup! : (-> (Mutable-Vectorof Index) Nonnegative-Fixnum Index Index Index Void)
  (lambda [fs sift-idx root root-key bottom-idx]
    (define l-idx : Nonnegative-Fixnum (+ (unsafe-idx+ sift-idx sift-idx) 1))
    
    (define-values (target target-key target-idx)
      (cond [(< l-idx bottom-idx) ; the right child is found, and select the smaller one
             (let* ([left (unsafe-vector*-ref fs l-idx)]
                    [l-key (huffman-minheap-key fs left)]
                    [r-idx (unsafe-idx+ l-idx 1)]
                    [right (unsafe-vector*-ref fs r-idx)]
                    [r-key (huffman-minheap-key fs right)])
               (if (< l-key r-key)
                   (values left l-key l-idx)
                   (values right r-key r-idx)))]
            [(= l-idx bottom-idx)
             (let* ([left (unsafe-vector*-ref fs l-idx)]
                    [l-key (huffman-minheap-key fs left)])
               (values left l-key l-idx))]
            [else #| in case the heap is not ruined |#
             (values root root-key 0 #| <= this `target-idx` is useless |#)]))
    
    (if (<= root-key target-key) ; `=` is necessary for the lazy swapping
        (begin
          ; the sifting code is home, and also don't forget to finish the lazy swapping.
          (unsafe-vector*-set! fs sift-idx root))
        (begin
          ; The sifting code is not the smallest one,
          ;   but we find which one should be moved to the position for it.
          ;   then retry with the smaller child.
          ; Yes, the swapping is delayed, too.
          (unsafe-vector*-set! fs sift-idx target)   
          (huffman-minheap-siftup! fs target-idx root root-key bottom-idx)))))

;;; the first phase, creation
; To repeatedly increase the size of the heap by 1
; by extending from the parent of last code to the first one, in each step:
;   do sift downwards so that the smallest code is at the root.
; After this phase, all other codes will be eventually in heap order.  
(define huffman-minheapify! : (-> (Mutable-Vectorof Index) Index Void)
  (lambda [fs n]
    (define bottom-idx : Index (unsafe-idx- n 1))
    
    (let heapify ([heapify-idx : Fixnum (- (quotient n 2) 1)])
      (when (>= heapify-idx 0)
        (let* ([root (unsafe-vector*-ref fs heapify-idx)])
          (huffman-minheap-siftup! fs heapify-idx root (huffman-minheap-key fs root) bottom-idx)
          (heapify (- heapify-idx 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-huffman-lookup-table : (-> (Immutable-Vectorof Byte) Index Index (Vectorof Index) (Vectorof Byte)
                                        (Values (Option Huffman-Lookup-Table) Index Boolean))
  (lambda [bits total simple-count bases extras]
    (values #false 0 #true)))
