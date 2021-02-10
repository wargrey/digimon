#lang typed/racket/base

;;; TAOCP Vol. 3, P145
;;; Managing Gigabytes: Compressing and Indexing Documents and Images, S2.3
;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/grub/grub-0.97/stage2/gunzip.c
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out) force)
(provide (all-from-out "table/huffman.rkt"))

(require (for-syntax racket/base))

(require racket/promise)

(require digimon/bitstream)

(require "table/huffman.rkt")

(require "../unsafe/ops.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXED HUFFMAN CODEWORDS
(define huffman-fixed-literal-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) upcodes)])
           (huffman-codewords-canonicalize! codewords huffman-fixed-literal-lengths huffman-fixed-literal-maxlength)
           codewords)))

(define huffman-fixed-distance-codewords : (Promise (Vectorof Index))
  (delay (let ([codewords ((inst make-vector Index) (vector-length huffman-fixed-distance-lengths))])
           (huffman-codewords-canonicalize! codewords huffman-fixed-distance-lengths huffman-fixed-distance-maxlength)
           codewords)))

(define huffman-codewords-canonicalize! : (->* ((Mutable-Vectorof Index) (Vectorof Index) Byte) ((Mutable-Vectorof Index) Index Index) Void)
  (lambda [codewords lengths maxlength [nextcodes ((inst make-vector Index) (+ maxlength maxlength) 0)] [start 0] [end (vector-length lengths)]]
    (define length-count-idx : Index maxlength)

    (vector-fill! codewords 0)
    (vector-fill! nextcodes 0)
    
    (let count-each-length ([idx : Nonnegative-Fixnum start])
      (when (< idx end)
        (define self-length : Index (unsafe-vector*-ref lengths idx))

        (when (> self-length 0)
          ; count[lengths[idx]]++;
          (let ([cidx (unsafe-idx+ length-count-idx self-length)])
            (unsafe-vector*-set! nextcodes cidx (unsafe-idx+ (unsafe-vector*-ref nextcodes cidx) 1))))

        (count-each-length (+ idx 1))))

    (let initialize-nextcode ([bitsize : Index 0])
      (when (< bitsize maxlength)
        (define bitsize++ : Index (+ bitsize 1))

        ; firstcode[l + 1] = (firstcode[l] + count[l]) << 1
        (unsafe-vector*-set! nextcodes bitsize++
                             (unsafe-idxlshift (+ (unsafe-vector*-ref nextcodes bitsize)
                                                  (unsafe-vector*-ref nextcodes (+ length-count-idx bitsize)))
                                               1))
        (initialize-nextcode bitsize++)))

    (let assign-codeword ([idx : Nonnegative-Fixnum start])
      (when (< idx end)
        (define self-length : Index (unsafe-vector*-ref lengths idx))

        (when (> self-length 0)
          ; [LSB] codeword[i] = nextcode[l]++
          (let ([self-code (unsafe-vector*-ref nextcodes self-length)])
            (unsafe-vector*-set! codewords (unsafe-idx- idx start) (bits-reverse-uint16 self-code self-length))
            (unsafe-vector*-set! nextcodes self-length (unsafe-idx+ self-code 1))))

        (assign-codeword (+ idx 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct huffman-lookup-table
  ([extra : Byte]
   [base : Byte]
   [secondary : (U Byte Huffman-Lookup-Table)])
  #:type-name Huffman-Lookup-Table
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HUFFMAN TREE
; WARNING: the heap starts from 0, which is different from the "common" implementations.

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

;;; Phase 0: Initialization
; To restore the heap storage (represented as a vector) so that
;   the first half accommodates the heap pointers, and
;   the second half accommodates the frequencies to be linked by heap pointers.
(define huffman-refresh-minheap! : (-> (Mutable-Vectorof Index) (Mutable-Vectorof Index) Index)
  (lambda [freqs heap]
    (define fcount : Index (vector-length freqs))
    (define fend : Index (unsafe-idx+ upcodes fcount))

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

;;; Phase 1: Creation (`heapify` is more accurate since it occurs in place)
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

;;; Phase 2: Shrinking
; To repeatedly decrease the size of the heap by 1
; by combining the two least frequent pointers (m1 and m2) until only one left, in each step
;   the combined pointer will become a new pointer in the heap and serve as the logical parent
;   of the two in the constructing huffman tree, and the heap order should be maintained both
;   before and after the combination since the heap might be ruined by steps.
; The idea is the same as that applied in the `heapsort`, with a much more complicated process.
; After this phase, the first value of the heap is the unique heap pointer to the root of the
;   tree, and the rest values are tree pointers to their corresponding parent. Specifically,
;   all internal trees are accommodated in the first half partion of the heap.
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
                 
                 ; original two frequencies are no longer used, link them to their logical parent in the tree
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

;;; Phase 3: Counting
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-huffman-lookup-table : (-> (Immutable-Vectorof Byte) Index Index (Vectorof Index) (Vectorof Byte)
                                        (Values (Option Huffman-Lookup-Table) Index Boolean))
  (lambda [bits total simple-count bases extras]
    (values #false 0 #true)))
