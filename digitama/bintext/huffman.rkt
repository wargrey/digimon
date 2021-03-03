#lang typed/racket/base

;;; TAOCP Vol. 3, P145
;;; Managing Gigabytes: Compressing and Indexing Documents and Images, S2.3
;;; On the implementation of minimum redundancy prefix codes
;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/grub/grub-0.97/stage2/gunzip.c
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out) force)
(provide (all-from-out "table/huffman.rkt"))

(require (for-syntax racket/base))

(require racket/promise)
(require racket/math)

(require digimon/bitstream)

(require "table/huffman.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define window-ibits : Positive-Byte 15)     ; bits of the window size that at least 32K for reading zip
(define window-obits : Positive-Byte 15)     ; bits of the window size that at most 32K for writing zip

(define upbits : Positive-Byte 16)           ; maximum bit length of any code (16 for explode)
(define upcodes : Positive-Index 288)        ; maximum number of codes in any set, bytes + backreferences + EOB
(define EOB : Index #;256 #x100)             ; end of (huffman) block

(define bit-order : (Immutable-Vectorof Byte) ; Order of the bit length code lengths
  (vector-immutable 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXED HUFFMAN SYMBOLS AND CODEWORDS
; WARNING: all vectors are 0-based, in which `head-offsets` and `sentinels` are different from those in "common" implementations.

; NOTE
; in case you are confused by arguments' names:
;   from the perspective of client applications,
;     "symbols" have to be arranged in order, so that
;     compression algorithms don't have to know concrete
;     symbols, but their consecutive "indices".
;   in terms of compression,
;     "symbols" are codes for original plain text;
;     "codewords" are codes for encoded bitstream.
;   for decoder, "codewords" are introduced to be referred by
;     their internal indices simply known as "symbol-indices".
;
;   That is to say, the decoder is nothing but a function
;     that maps a "symbol index", which is resolved from
;     the compressed bitstream, to its original symbol "index".

(struct huffman-lookup-table
  ([cheatsheet : (Mutable-Vectorof Index)]    ; see `pad-cheatsheet` in `huffman-lookup-table-canonicalize!`
   [head-offsets : (Mutable-Vectorof Fixnum)] ; offsets from the headcode of length
   [sentinels : (Mutable-Vectorof Index)]     ; max value(MSB, exclusive) of codeword of length
   [symbols : (Mutable-Vectorof Index)]
   [cheat-bwidth : Byte]
   [cheat-mask : Index]
   [symbol-bwidth : Byte]
   [symbol-mask : Index])
  #:type-name Huffman-Lookup-Table)

(define huffman-make-lookup-table : (-> [#:fast-lookup-bits Byte] [#:max-bitwidth Byte] [#:symbol-capacity Index] Huffman-Lookup-Table)
  (lambda [#:fast-lookup-bits [cheat-bwidth 8] #:max-bitwidth [maxlength upbits] #:symbol-capacity [max-indices upcodes]]
    (define symbol-bwidth : Byte (assert (exact-ceiling (log max-indices 2)) byte?))
    (define symbol-mask : Index (- (unsafe-idxlshift 1 symbol-bwidth) 1))
    (define cheat-size : Positive-Index (unsafe-idxlshift 1 cheat-bwidth))
    
    (huffman-lookup-table ((inst make-vector Index) cheat-size 0)
                          ((inst make-vector Fixnum) maxlength)
                          ((inst make-vector Index) maxlength)
                          ((inst make-vector Index) max-indices)
                          cheat-bwidth (- cheat-size 1)
                          symbol-bwidth symbol-mask)))

(define huffman-fixed-literal-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) upcodes)])
           (huffman-codewords-canonicalize! codewords huffman-fixed-literal-lengths huffman-fixed-literal-maxlength)
           codewords)))

(define huffman-fixed-distance-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) (vector-length huffman-fixed-distance-lengths))])
           (huffman-codewords-canonicalize! codewords huffman-fixed-distance-lengths huffman-fixed-distance-maxlength)
           codewords)))

(define huffman-fixed-literal-lookup-table : (Promise Huffman-Lookup-Table)
  (delay (let ([table (huffman-make-lookup-table #:fast-lookup-bits (min huffman-fixed-literal-maxlength 8))])
           (huffman-lookup-table-canonicalize! table huffman-fixed-literal-lengths huffman-fixed-literal-maxlength)
           table)))

(define huffman-fixed-distance-lookup-table : (Promise Huffman-Lookup-Table)
  (delay (let ([table (huffman-make-lookup-table #:fast-lookup-bits (min huffman-fixed-distance-maxlength 8))])
           (huffman-lookup-table-canonicalize! table huffman-fixed-distance-lengths huffman-fixed-distance-maxlength)
           table)))

;; Canonicalize huffman codewords of lengths stored in `lengths` from `start` to `end`,
;    with `nextcodes` and `counts`, which accommodate temporary data, just in case clients don't want them to be allocated every time.
;    also note that, `maxlength` means that lengths range in the closed interval [0, maxlength], hence `add1`s.
(define huffman-codewords-canonicalize! : (->* ((Mutable-Vectorof Index) (Vectorof Index) Byte)
                                               ((Mutable-Vectorof Index) (Mutable-Vectorof Index) Index Index)
                                               Void)
  (lambda [codewords lengths maxlength
                     [nextcodes ((inst make-vector Index) (add1 maxlength) 0)] [counts ((inst make-vector Index) (add1 maxlength) 0)]
                     [start 0] [end (vector-length lengths)]]

    (let count-each-length ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define self-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> self-length 0) (<= self-length maxlength))
          ; count[lengths[idx]]++;
          (unsafe-vector*-set! counts self-length
                               (unsafe-idx+ (unsafe-vector*-ref counts self-length) 1)))

        (count-each-length (+ cursor-idx 1))))

    (unsafe-vector*-set! nextcodes 0 0)
    (unsafe-vector*-set! counts 0 0)

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
;    also note that, `maxlength` means that lengths range in the closed interval [0, maxlength], hence `add1`s.
(define huffman-lookup-table-canonicalize! : (->* (Huffman-Lookup-Table (Vectorof Index) Byte)
                                                  (#:on-error (Option (-> Any))
                                                   (Mutable-Vectorof Index) (Mutable-Vectorof Index) (Mutable-Vectorof Index) Index Index)
                                                  Void)
  (lambda [table lengths maxlength #:on-error [oops! #false]
                 [symbol-indices ((inst make-vector Index) (add1 maxlength) 0)]
                 [counts ((inst make-vector Index) (add1 maxlength) 0)]
                 [codes ((inst make-vector Index) (add1 maxlength) 0)]
                 [start 0] [end (vector-length lengths)]]
    (define cheatsheet : (Mutable-Vectorof Index) (huffman-lookup-table-cheatsheet table))
    (define symbols : (Mutable-Vectorof Index) (huffman-lookup-table-symbols table))
    (define headoffs : (Mutable-Vectorof Fixnum) (huffman-lookup-table-head-offsets table))
    (define sentinels : (Mutable-Vectorof Index) (huffman-lookup-table-sentinels table))
    (define cheat-bwidth : Byte (huffman-lookup-table-cheat-bwidth table))
    (define symbol-bwidth : Byte (huffman-lookup-table-symbol-bwidth table))
    
    (vector-fill! cheatsheet 0)

    ; same as in `huffman-codewords-canonicalize!`
    (let count-each-length ([cursor-idx : Nonnegative-Fixnum start])
      (when (< cursor-idx end)
        (define self-length : Index (unsafe-vector*-ref lengths cursor-idx))

        (when (and (> self-length 0) (<= self-length maxlength))
          ; count[lengths[idx]]++;
          (unsafe-vector*-set! counts self-length
                               (unsafe-idx+ (unsafe-vector*-ref counts self-length) 1)))

        (count-each-length (+ cursor-idx 1))))

    (unsafe-vector*-set! symbol-indices 0 0)
    (unsafe-vector*-set! codes 0 0)
    (unsafe-vector*-set! counts 0 0)

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
              ; <=> end-1 > (0b1000... - 1) = 0b111...
              (oops!))))

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
; NOTE that the `Huffman-Lookup-Table` is intentionally designed for clients to reuse memory,
;   hence the third optional argument, in case the actual payload of the table is smaller than the capacity. 
(define huffman-symbol-extract : (->* (Huffman-Lookup-Table Index) (Byte) (Values Index Byte))
  (lambda [table codeword+more [codeword-bwidth upbits]]
    (define cheatsheet : (Mutable-Vectorof Index) (huffman-lookup-table-cheatsheet table))
    (define cheat-idx : Index ; no special transform for LSB
      (cond [(< codeword+more (vector-length cheatsheet)) codeword+more]
            [else (bitwise-and codeword+more (huffman-lookup-table-cheat-mask table))]))
    (define cheat-code : Index (unsafe-vector*-ref cheatsheet cheat-idx))

    (if (> cheat-code 0)
        
        ; see `pad-cheatsheet` in `huffman-lookup-table-canonicalize!`
        (values (bitwise-and cheat-code (huffman-lookup-table-symbol-mask table))
                (unsafe-brshift cheat-code (huffman-lookup-table-symbol-bwidth table)))
        
        ; manually do canonical decoding with the bits in MSB-first order
        (let* ([prefix-code (bits-reverse-uint16 codeword+more codeword-bwidth)])
          ;; NOTE
          ; In zip implementation, the number of search is usually very small,
          ;   thus, binary search is not a neccesity here.
          (let linear-search ([code-length : Nonnegative-Fixnum (+ (huffman-lookup-table-cheat-bwidth table) 1)])
            (if (<= code-length codeword-bwidth)

                ; Also recall that both `sentinels` and `head-offsets` are 0-base vectors
                (let ([0-base-length-idx (- code-length 1)])
                  (if (< prefix-code (unsafe-vector*-ref (huffman-lookup-table-sentinels table) 0-base-length-idx))
                      (let* ([dropping-bitsize (unsafe-idx- codeword-bwidth code-length)]
                             [nbits-code (if (> dropping-bitsize 0) (unsafe-idxrshift prefix-code dropping-bitsize) prefix-code)]
                             [head-offset (unsafe-vector*-ref (huffman-lookup-table-head-offsets table) 0-base-length-idx)])
                        (values (unsafe-vector*-ref (huffman-lookup-table-symbols table) (unsafe-fx+ head-offset nbits-code))
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
    (define fend : Index (unsafe-idx+ upcodes fcount))

    ;;; WARNING
    ; Don't squeeze out 0-frequent symbols
    ;   since they are mapped to indices so that
    ;   we don't have to maintain them separately.
    (vector-copy! heap upcodes freqs 0 fcount)

    (let heap-link! ([f-idx : Nonnegative-Fixnum upcodes]
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
; After this phase, the second half partion of the heap accommodates all desired lengths.
(define huffman-minheap-count-lengths! : (-> (Mutable-Vectorof Index) Index Byte)
  (lambda [heap n]
    (define end : Index (vector-length heap))
       
    (vector-set! heap 1 0) ; root is 0
    
    (let update-depths ([idx : Nonnegative-Fixnum 2]
                        [maxlength : Nonnegative-Fixnum 0])
      (cond [(>= idx end) (assert maxlength byte?)]
            [(and (< idx upcodes) (>= idx n)) (update-depths upcodes maxlength)] ; skip dirty memory
            [else (let ([parent-idx (vector-ref heap idx)]
                        [idx++ (+ idx 1)])
                    (cond [(= parent-idx 0) (update-depths idx++ maxlength)]
                          [else (let ([length (unsafe-idx+ (vector-ref heap parent-idx) 1)])
                                  (vector-set! heap idx length)
                                  (update-depths idx++ (max maxlength length)))]))]))))

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
