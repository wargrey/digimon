#lang typed/racket/base

;;; TAOCP Vol. 3, P145
;;; Managing Gigabytes: Compressing and Indexing Documents and Images, S2.3
;;; On the implementation of minimum redundancy prefix codes
;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/grub/grub-0.97/stage2/gunzip.c
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

; NOTE
; in case you are confused by arguments' names:
;   from the perspective of client applications,
;     "symbols" have to be arraged in order, so that
;     compression algorithms don't have to work on concrete
;     symbols, but their consecutive "indices" starting from 0.
;   in terms of compression,
;     "symbols" are codes for original plain text;
;     "codewords" are codes for compressed bitstream.
;   for the decoder, "codewords" are introduced to be referred by
;     their internal indices simply known as "symbol-indices".
;
;   That is to say, the decoder is nothing but a function
;     that maps a "symbol index", which is resolved from
;     the compressed bitstream, to corresponding symbol's "index".

(provide (all-defined-out) force)
(provide (all-from-out "table/huffman.rkt"))

(require (for-syntax racket/base))

(require racket/promise)
(require racket/math)

(require digimon/bitstream)

(require "table/huffman.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define window-ibits : Positive-Byte 15)      ; bits of the window size that at least 32K for reading zip
(define window-obits : Positive-Byte 15)      ; bits of the window size that at most 32K for writing zip

(define upbits : Positive-Byte 16)            ; maximum bit length of any code (16 for explode)
(define EOB : Index #;256 #x100)              ; end of (huffman) block
(define EOBbits : Byte (unsafe-vector*-ref huffman-fixed-literal-lengths EOB))

(define uplitcodes : Index (vector-length huffman-fixed-literal-lengths))
(define strict-uplitcodes : Index (unsafe-idx+ backref-span-offset (vector-length huffman-backref-bases)))

(define updistcodes : Index (vector-length huffman-fixed-distance-lengths))
(define strict-updistcodes : Index (vector-length huffman-distance-bases))

; these `nbase`s are designed for reducing the bits of dynamic block header by substracting them before encoding
(define literal-nbase : Index backref-span-offset)
(define distance-nbase : Byte 1)
(define codelen-nbase : Byte 4)

; as intricate as it is, the "codelen codes" are "codewords" for
;   encoding the lengths of "codewords" of the dynamic huffman codes,
;   and these codes themselves are encoded and stored in the block,
;   in the order:
(define codelen-lengths-order : Bytes (bytes 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
(define uplencodes : Index (bytes-length codelen-lengths-order))
(define uplenbits : Positive-Byte #b111)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXED HUFFMAN SYMBOLS AND CODEWORDS
; WARNING: all vectors are 0-based, in which `head-offsets` and `sentinels` are different from those in "common" implementations.

(struct huffman-alphabet
  ([cheatsheet : (Mutable-Vectorof Index)]    ; see `pad-cheatsheet` in `huffman-alphabet-canonicalize!`
   [head-offsets : (Mutable-Vectorof Fixnum)] ; offsets from the headcode of length
   [sentinels : (Mutable-Vectorof Index)]     ; max value(MSB, exclusive) of codeword of length
   [symbols : (Mutable-Vectorof Index)]
   [cheat-bwidth : Byte]
   [cheat-mask : Index]
   [symbol-bwidth : Byte]
   [symbol-mask : Index]
   [max-bwidth : Byte])
  #:type-name Huffman-Alphabet)

; note the keyword `#:max-bitwidth`, which means the actual `maxlength` might be smaller (but not larger)
; since the `huffman-alphabet` is intentionally designed to allow client applications reusing the memory.
(define huffman-make-alphabet : (->* () (Index #:max-bitwidth Byte #:cheat-bitwidth Byte) Huffman-Alphabet)
  (lambda [[capacity uplitcodes] #:max-bitwidth [maxlength upbits] #:cheat-bitwidth [cheat-bwidth (min 8 maxlength)]]
    (define symbol-bwidth : Byte (assert (exact-ceiling (log capacity 2)) byte?))
    (define symbol-mask : Index (- (unsafe-idxlshift 1 symbol-bwidth) 1))
    (define cheat-size : Positive-Index (unsafe-idxlshift 1 cheat-bwidth))
    
    (huffman-alphabet ((inst make-vector Index) cheat-size 0)
                      ((inst make-vector Fixnum) maxlength 0)
                      ((inst make-vector Index) maxlength 0)
                      ((inst make-vector Index) capacity 0)
                      cheat-bwidth (- cheat-size 1)
                      symbol-bwidth symbol-mask
                      maxlength)))

(define huffman-fixed-literal-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) uplitcodes)])
           (huffman-codewords-canonicalize! codewords huffman-fixed-literal-lengths huffman-fixed-literal-maxlength)
           codewords)))

(define huffman-fixed-distance-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) (vector-length huffman-fixed-distance-lengths))])
           (huffman-codewords-canonicalize! codewords huffman-fixed-distance-lengths huffman-fixed-distance-maxlength)
           codewords)))

(define huffman-fixed-literal-alphabet : (Promise Huffman-Alphabet)
  (delay (let ([a (huffman-make-alphabet #:max-bitwidth huffman-fixed-literal-maxlength)])
           (huffman-alphabet-canonicalize! a huffman-fixed-literal-lengths)
           a)))

(define huffman-fixed-distance-alphabet : (Promise Huffman-Alphabet)
  (delay (let ([a (huffman-make-alphabet #:max-bitwidth huffman-fixed-distance-maxlength)])
           (huffman-alphabet-canonicalize! a huffman-fixed-distance-lengths)
           a)))

;; Canonicalize huffman codewords of lengths stored in `lengths` from `start` to `end`,
;    with `nextcodes` and `counts`, which accommodate temporary data, just in case clients don't want them to be allocated every time.
;    also note that, `maxlength` means that lengths range in the closed interval [0, maxlength], hence `add1`s.
(define huffman-codewords-canonicalize! : (->* ((Mutable-Vectorof Index) (Vectorof Index) Byte)
                                               ((Mutable-Vectorof Index) (Mutable-Vectorof Index) Index Index)
                                               Void)
  (lambda [codewords lengths maxlength
                     [nextcodes ((inst make-vector Index) (add1 maxlength) 0)] [counts ((inst make-vector Index) (add1 maxlength) 0)]
                     [start 0] [end (vector-length lengths)]]
    (unsafe-vector*-set! nextcodes 0 0) ; exclude zero lengths
    (vector-fill! counts 0) ; if data in `counts` is clean, dirty data in other vectors won't be harmful 
    
    (let count-each-length ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define self-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> self-length 0) (<= self-length maxlength))
          ; count[lengths[idx]]++;
          ; note that `lengths[idx]` might not be consecutive, so that `counts` must be reset first
          (unsafe-vector*-set! counts self-length
                               (unsafe-idx+ (unsafe-vector*-ref counts self-length) 1)))

        (count-each-length (+ cursor-idx 1))))

    (let initialize-nextcodes ([len : Index 0])
      (when (< len maxlength)
        (define len+1 : Index (+ len 1))

        ; headcodes are just initial nextcodes.
        ; headcode[l + 1] = (headcode[l] + count[l]) << 1
        (unsafe-vector*-set! nextcodes len+1
                             (unsafe-idxlshift (+ (unsafe-vector*-ref nextcodes len)
                                                  (unsafe-vector*-ref counts len))
                                               1))
        (initialize-nextcodes len+1)))
    
    ;;; NOTE
    ; The codewords are actually not huffman codes at all, nevertheless, they work equivalently,
    ;   as the huffman codes is a subset of certain minimum redundancy codes.
    (let resolve-codewords ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define code-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> code-length 0) (<= code-length maxlength))
          ; [LSB] codeword[i] = nextcode[l]++
          (let ([self-code (unsafe-vector*-ref nextcodes code-length)])
            (unsafe-vector*-set! codewords (unsafe-idx- cursor-idx start) (bits-reverse-uint16 self-code code-length))
            (unsafe-vector*-set! nextcodes code-length (unsafe-idx+ self-code 1))))

        (resolve-codewords (+ cursor-idx 1))))))

;; Canonicalize the lookup table of lengths stored in `lengths` from `start` to `end` for decoding huffman codewords,
;    with `indices`, `counts` and `headcodes`, which accommodate temporary data, just in case clients don't want them to be allocated every time.
;  Also note that, `maxlength`, which is the smaller one between `strict-maxlength` and `max-bwidth` of the alphabet,
;    means that lengths range in the closed interval [0, maxlength], hence `add1`s.
(define huffman-alphabet-canonicalize! : (->* (Huffman-Alphabet (Vectorof Index))
                                              (#:strict-bitwidth Byte #:on-error (Option (Index -> Any))
                                               (Mutable-Vectorof Index) (Mutable-Vectorof Index) (Mutable-Vectorof Index) Index Index)
                                              Void)
  (lambda [alphabet lengths #:strict-bitwidth [strict-maxlength (huffman-alphabet-max-bwidth alphabet)] #:on-error [oops! #false]
                    [symbol-indices ((inst make-vector Index) (add1 strict-maxlength) 0)]
                    [counts ((inst make-vector Index) (add1 strict-maxlength) 0)]
                    [codes ((inst make-vector Index) (add1 strict-maxlength) 0)]
                    [start 0] [end (vector-length lengths)]]
    (define cheatsheet : (Mutable-Vectorof Index) (huffman-alphabet-cheatsheet alphabet))
    (define symbols : (Mutable-Vectorof Index) (huffman-alphabet-symbols alphabet))
    (define headoffs : (Mutable-Vectorof Fixnum) (huffman-alphabet-head-offsets alphabet))
    (define sentinels : (Mutable-Vectorof Index) (huffman-alphabet-sentinels alphabet))
    (define cheat-bwidth : Byte (huffman-alphabet-cheat-bwidth alphabet))
    (define symbol-bwidth : Byte (huffman-alphabet-symbol-bwidth alphabet))
    (define maxlength : Byte (min strict-maxlength (huffman-alphabet-max-bwidth alphabet)))

    ; exclude zero lengths
    (unsafe-vector*-set! symbol-indices 0 0)
    (unsafe-vector*-set! codes 0 0)
    
    ; if data in these vectors are clean
    ; dirty data in other vectors won't be harmful 
    (vector-fill! cheatsheet 0)
    (vector-fill! sentinels 0)
    
    ; same as in `huffman-codewords-canonicalize!`
    (vector-fill! counts 0)
    (let count-each-length ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define self-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> self-length 0) (<= self-length maxlength))
          ; count[lengths[idx]]++;
          ; note that `lengths[idx]` might not be consecutive, so that `counts` must be reset first
          (unsafe-vector*-set! counts self-length
                               (unsafe-idx+ (unsafe-vector*-ref counts self-length) 1)))

        (count-each-length (+ cursor-idx 1))))

    ;; WARNING
    ; Both `head-offsets` and `sentinels` here are 0-base vectors, and the `bitsize` starts from 0,
    ;   other "common" implementations might employ 1-base vectors, so please care the indices of them.
    (let resolve-coordinates ([len : Index 0])
      (when (< len maxlength)
        (define len+1 : Index (+ len 1))

        ; headcode[l + 1] = (headcode[l] + count[l]) << 1
        (unsafe-vector*-set! codes len+1
                             (unsafe-idxlshift (+ (unsafe-vector*-ref codes len)
                                                  (unsafe-vector*-ref counts len))
                                               1))

        (unless (not oops!)
          (when (> (unsafe-vector*-ref counts len+1) 0)
            (define exclusive-last-codeword : Nonnegative-Fixnum (+ (unsafe-vector*-ref codes len+1) (unsafe-vector*-ref counts len+1)))
            (define max-codeword-of-length+1 : Index (unsafe-idxlshift 1 len+1))

            (when (> exclusive-last-codeword max-codeword-of-length+1)
              ; <=> end > (0b1000... - 1) = 0b111...
              (oops! len+1))))

        ; sentinel[l + base] = (headcode[l + 1] + count[l + 1]) << (maxlength - (l + 1))
        ; so that all sentinels have the same length equalling to `maxlength`
        ;   except the last one which overflows to 1-bit longer.
        (unsafe-vector*-set! sentinels len
                             (unsafe-idxlshift (+ (unsafe-vector*-ref codes len+1)
                                                  (unsafe-vector*-ref counts len+1))
                                               (unsafe-idx- maxlength len+1)))

        ; symbol-index[l + 1] = symbol-index[l] + count[l]
        (unsafe-vector*-set! symbol-indices len+1
                             (unsafe-idx+ (unsafe-vector*-ref symbol-indices len)
                                          (unsafe-vector*-ref counts len)))
        
        ; head-offset[l + base] = index[l + 1] - headcode[l + 1]
        ; yes, offsets are nonpositive fixnums
        (unsafe-vector*-set! headoffs len
                             (unsafe-fx- (unsafe-vector*-ref symbol-indices len+1)
                                         (unsafe-vector*-ref codes len+1)))
        
        (resolve-coordinates len+1)))

    (let resolve-symbols ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define code-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> code-length 0) (<= code-length maxlength))
          (define idx : Index (unsafe-idx- cursor-idx start))
          (define pad-length : Fixnum (- cheat-bwidth code-length))

          ; symbol[symbol-index[l]++] = idx
          (let ([self-index (unsafe-vector*-ref symbol-indices code-length)])
            (unsafe-vector*-set! symbols self-index idx)
            (unsafe-vector*-set! symbol-indices code-length (unsafe-idx+ self-index 1)))

          (when (>= pad-length 0)
            (define self-code : Index (unsafe-vector*-ref codes code-length))
            (define codeword : Index (bits-reverse-uint16 self-code code-length))
            (define padmask+1 : Index (unsafe-idxlshift 1 pad-length))

            ; code[l]++
            (unsafe-vector*-set! codes code-length (unsafe-idx+ self-code 1))
            
            (let pad-cheatsheet ([padding : Nonnegative-Fixnum 0])
              (when (< padding padmask+1)
                (unsafe-vector*-set! cheatsheet

                                     ; say, suppose the max bit width is 3, after padding, an 1-bit codeword
                                     ;   takes 4 entries of the cheatsheet, 00x, 01x, 10x, and 11x.
                                     ; BTW, the name 'padmask+1' helps you understand this algorithm.
                                     (bitwise-ior codeword (unsafe-idxlshift padding code-length))

                                     ; the cheat code should be `(cons code-length idx)`,
                                     ;   but these are all small integers (a.k.a fixnums) even in a 32-bit system,
                                     ;   in which case compiler will generate more efficient code for them instead
                                     ;   of allocating lots of momery for pairs.
                                     (bitwise-xor (unsafe-idxlshift code-length symbol-bwidth) idx))
                
                (pad-cheatsheet (+ padding 1))))))

        (resolve-symbols (+ cursor-idx 1))))))

;; Extract one symbol from current bitstream
; NOTE that the `Huffman-Alphabet` is intentionally designed for clients to reuse memory,
;   hence the third optional argument, in case the actual payload of the alphabet is smaller than the capacity. 
(define huffman-symbol-extract : (->* (Huffman-Alphabet Index) (Byte) (Values Index Byte))
  (lambda [alphabet codeword+more [codeword-bwidth (huffman-alphabet-max-bwidth alphabet)]]
    (define cheatsheet : (Mutable-Vectorof Index) (huffman-alphabet-cheatsheet alphabet))
    (define cheat-idx : Index ; no special transform for LSB-first codes
      (cond [(< codeword+more (vector-length cheatsheet)) codeword+more]
            [else (bitwise-and codeword+more (huffman-alphabet-cheat-mask alphabet))]))
    (define cheat-code : Index (unsafe-vector*-ref cheatsheet cheat-idx))

    (if (> cheat-code 0)
        
        ; see `pad-cheatsheet` in `huffman-alphabet-canonicalize!`
        (values (bitwise-and cheat-code (huffman-alphabet-symbol-mask alphabet))
                (unsafe-brshift cheat-code (huffman-alphabet-symbol-bwidth alphabet)))
        
        ; manually do canonical decoding with the bits in MSB-first order
        (let* ([prefix-code (bits-reverse-uint16 codeword+more codeword-bwidth)])
          ;; NOTE
          ; In zip implementation, the number of search is usually very small,
          ;   thus, binary search is not a neccesity here.
          (let linear-search ([code-length : Nonnegative-Fixnum (+ (huffman-alphabet-cheat-bwidth alphabet) 1)])
            (if (<= code-length codeword-bwidth)

                ; Also recall that both `sentinels` and `head-offsets` are 0-base vectors
                (let ([0-base-length-idx (- code-length 1)])
                  (if (< prefix-code (unsafe-vector*-ref (huffman-alphabet-sentinels alphabet) 0-base-length-idx))
                      (let* ([dropping-bitsize (unsafe-idx- codeword-bwidth code-length)]
                             [nbits-code (if (> dropping-bitsize 0) (unsafe-idxrshift prefix-code dropping-bitsize) prefix-code)]
                             [head-offset (unsafe-vector*-ref (huffman-alphabet-head-offsets alphabet) 0-base-length-idx)])
                        (values (unsafe-vector*-ref (huffman-alphabet-symbols alphabet) (unsafe-fx+ head-offset nbits-code))
                                code-length))
                      (linear-search (+ code-length 1))))

                #| not found |#
                (values 0 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HUFFMAN TREE
; WARNING: the heap starts from 0, which is different from that in "common" implementations.

(define-syntax (huffman-minheap-key stx)
  (syntax-case stx []
    [(_ heap pointer)
     (syntax/loc stx
       (unsafe-vector*-ref heap pointer))]))

(define-syntax (huffman-minheap-parent stx)
  (syntax-case stx []
    [(_ idx)
     (syntax/loc stx
       (quotient (unsafe-idx- idx 1) 2))]))

;; Phase 0: Initialization
; To restore the heap storage (represented as a vector) so that
;   the first half accommodates the heap pointers, and
;   the second half accommodates the frequencies to be linked by heap pointers.
(define huffman-refresh-minheap! : (-> (Mutable-Vectorof Index) (Mutable-Vectorof Index) Index)
  (lambda [freqs heap]
    (define fcount : Index (vector-length freqs))
    (define fend : Index (unsafe-idx+ uplitcodes fcount))

    ;;; WARNING
    ; Don't squeeze out 0-frequent symbols
    ;   since they are mapped to indices so that
    ;   we don't have to maintain them separately.
    (vector-copy! heap uplitcodes freqs 0 fcount)

    (let heap-link! ([f-idx : Nonnegative-Fixnum uplitcodes]
                     [n : Index 0])
      (cond [(>= f-idx fend) n]
            [else (let ([freq (vector-ref heap f-idx)]
                        [i++ (add1 f-idx)])
                    (cond [(= freq 0) (heap-link! i++ n)]
                          [else (let ([n++ (unsafe-idx+ n 1)])
                                  (vector-set! heap n f-idx)
                                  (heap-link! i++ n++))]))]))))

;; Phase 1: Creation (`heapify` is more accurate since it occurs in place)
; To repeatedly increase the size of the heap by 1
; by extending from the parent of last pointer to the first one, in each step:
;   do sift downwards so that the least frequent pointer is at the root.
; After this phase, all other pointers will be eventually in heap order.  
(define huffman-minheapify! : (-> (Mutable-Vectorof Index) Index Void)
  (lambda [heap n]
    (define bottom-idx : Index (unsafe-idx- n 1))
    
    (let heapify ([heapify-idx : Fixnum (huffman-minheap-parent bottom-idx)])
      (when (>= heapify-idx 0)
        (let* ([root-pointer (unsafe-vector*-ref heap heapify-idx)])
          (huffman-minheap-siftup! heap heapify-idx root-pointer (huffman-minheap-key heap root-pointer) bottom-idx)
          (heapify (- heapify-idx 1)))))))

;; Phase 2: Shrinking
; To repeatedly decrease the size of the heap by 1
; by combining the two least frequent pointers (m1 and m2) until only one left, in each step
;   the combined pointer will become a new pointer in the heap and serve as the logical parent
;   of the two in the constructing huffman tree, and the heap order should be maintained both
;   before and after the combination since the heap might be ruined by steps.
; The idea is the same as that applied in the `heapsort`, with a more complicated process.
; After this phase, the first value of the heap is the unique heap pointer to the root of the
;   tree, and the rest values are tree pointers to their corresponding parent. Specifically,
;   all internal trees are accommodated in the first half portion of the heap.
(define huffman-minheap-treefy! : (-> (Mutable-Vectorof Index) Index Void)
  (lambda [heap n]
    (cond [(>= n 2)
           (let treefy ([bottom-idx : Index (unsafe-idx- n 1)])
             (when (> bottom-idx 0)
               (define b-idx-- : Index (unsafe-idx- bottom-idx 1))
               
               ;; Extract the least frequent pointer as in `heapsort`,
               ;    but the bottom position of the heap is reserved for the combined frequency
               ;    rather than certain sorting record. 
               (define m1 : Index (unsafe-vector*-ref heap 0))
               (let ([root-pointer (unsafe-vector*-ref heap bottom-idx)])
                 ;; NOTE that the original pointer at the bottom position should be moved into the root position,
                 ;    but for the sake of performance, this move is delayed since the root might not be its home.
                 (huffman-minheap-siftup! heap 0 root-pointer (huffman-minheap-key heap root-pointer) b-idx--))
               
               ;; Extract the 2nd least frequent pointer, and combine it and the previous one 
               (let* ([m2 (unsafe-vector*-ref heap 0)]
                      [combined-freq (unsafe-idx+ (unsafe-vector*-ref heap m1) (unsafe-vector*-ref heap m2))])
                 ; the reserved position accommodates the new frequency
                 (unsafe-vector*-set! heap bottom-idx combined-freq)
                 
                 ; original two frequencies are no longer used, link them to their logical parent
                 (unsafe-vector*-set! heap m1 bottom-idx)
                 (unsafe-vector*-set! heap m2 bottom-idx)
                 
                 (let ([root-pointer bottom-idx])
                   ; the root position accommodates the new least frequent pointer (which may or may not be the candidate)
                   ; after re-sifting, in which case the invoking of `unsafe-vector*-set!` is reasonably to be delayed as above. 
                   (huffman-minheap-siftup! heap 0 root-pointer combined-freq b-idx--)))
               
               (treefy b-idx--)))]
          [(= n 1) ; tiny heap should also stick to the spec
           (unsafe-vector*-set! heap (unsafe-vector*-ref heap 0) 1)
           (unsafe-vector*-set! heap 0 1)])))

;; Phase 3: Counting
; To count the lengths of each codewords and return the max one in case it is too long.
; The 2nd phase ensures that children are always accommodated after their parents,
;   so that we don't have to thoroughly count lengths manually.
; After this phase, the second half portion of the heap accommodates all desired lengths.
(define huffman-minheap-count-lengths! : (-> (Mutable-Vectorof Index) Index Byte)
  (lambda [heap n]
    (define end : Index (vector-length heap))
       
    (vector-set! heap 1 0) ; root is 0
    
    (let update-depths ([idx : Nonnegative-Fixnum 2]
                        [maxlength : Byte 0])
      (cond [(>= idx end) maxlength]
            [(and (< idx uplitcodes) (>= idx n)) (update-depths uplitcodes maxlength)] ; skip dirty memory
            [else (let ([parent-idx (unsafe-vector*-ref heap idx)]
                        [idx++ (+ idx 1)])
                    (cond [(= parent-idx 0) (update-depths idx++ maxlength)]
                          [else (let ([length (unsafe-b+ (unsafe-vector*-ref heap parent-idx) 1)])
                                  (unsafe-vector*-set! heap idx length)
                                  (update-depths idx++ (unsafe-fxmax maxlength length)))]))]))))

; the name "siftup" is a little confusing,
;   it moves up the least frequent pointer which is originally located at the bottom, but
;   the sift procedure is actually known as the top-down approach, and might be given
;   name with "siftDown", in which case the "down" should be replaced by "downwards".
(define huffman-minheap-siftup! : (-> (Mutable-Vectorof Index) Nonnegative-Fixnum Index Index Index Void)
  (lambda [heap sift-idx root-pointer root-key bottom-idx]
    (define l-idx : Nonnegative-Fixnum (+ (unsafe-idx+ sift-idx sift-idx) 1))
    
    (define-values (target target-key target-idx)
      (cond [(< l-idx bottom-idx) ; the right child is found, and select the less frequent one
             (let* ([left (unsafe-vector*-ref heap l-idx)]
                    [l-key (huffman-minheap-key heap left)]
                    [r-idx (unsafe-idx+ l-idx 1)]
                    [right (unsafe-vector*-ref heap r-idx)]
                    [r-key (huffman-minheap-key heap right)])
               (if (< l-key r-key)
                   (values left l-key l-idx)
                   (values right r-key r-idx)))]
            [(= l-idx bottom-idx)
             (let* ([left (unsafe-vector*-ref heap l-idx)]
                    [l-key (huffman-minheap-key heap left)])
               (values left l-key l-idx))]
            [else #| in case the heap is not ruined |#
             (values root-pointer root-key 0 #| <= this `target-idx` is useless |#)]))
    
    (if (<= root-key target-key) ; `=` is necessary for the lazy move
        (begin
          ; the sifting pointer is home, and also don't forget to finish the lazy move.
          (unsafe-vector*-set! heap sift-idx root-pointer))
        (begin
          ; The sifting pointer is not the least frequent one,
          ;   but we find which one should be moved to the position for it.
          ;   then retry with the smaller child.
          ; Yes, the move is delayed, too.
          (unsafe-vector*-set! heap sift-idx target)   
          (huffman-minheap-siftup! heap target-idx root-pointer root-key bottom-idx)))))
