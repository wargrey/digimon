#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://www.euccas.me/zlib
;;; https://www.intel.com/content/dam/www/public/us/en/documents/white-papers/zlib-compression-whitepaper-copy.pdf

(provide (all-defined-out))

(require "zipconfig.rkt")

(require "../unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LZ77-Select-Codeword
  (case-> [Byte Index -> Void]
          [Index Index Index -> Void]))

(define lz77-default-hash-bits : Positive-Byte 15)
(define lz77-default-min-match : Positive-Byte 3)
(define lz77-default-max-match : Index 258)
(define lz77-default-farthest : Index 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (lz77-accumulative-hash stx)
  (syntax-case stx []
    [(_ heads hv b shift mask)
     (syntax/loc stx
       (let ([h++ (lz77-accumulative-hash hv b shift mask)])
         (values h++ (unsafe-vector*-ref heads h++))))]
    [(_ hv b shift mask)
     (syntax/loc stx
       (bitwise-and mask
                    (bitwise-xor (unsafe-idxlshift hv shift)
                                 b)))]))

(define-syntax (lz77-update-hash stx)
  (syntax-case stx []
    [(_ heads prevs hash0 b idx shift mask nil)
     (syntax/loc stx
       (let-values ([(h++ ?pointer) (lz77-accumulative-hash heads hash0 b shift mask)])
         (lz77-update-hash heads prevs h++ ?pointer idx nil)
         (values h++ ?pointer)))]
    [(_ heads prevs hash ?pointer idx nil)
     (syntax/loc stx
       (let ()
         (unsafe-vector*-set! heads hash idx)
         (unless (= ?pointer nil)
           (unsafe-vector*-set! prevs idx ?pointer))))]
    [(_ heads prevs hash idx nil)
     (syntax/loc stx
       (let ([?pointer (unsafe-vector*-ref heads hash)])
         (lz77-update-hash heads prevs hash ?pointer idx nil)))]))

(define-syntax (lz77-insert-string stx)
  (syntax-case stx []
    [(_ heads prevs hash0 window start end shift mask nil)
     (syntax/loc stx
       (let insert : Index ([idx : Index start]
                            [hash : Index hash0])
         (cond [(>= idx end) hash]
               [else (let-values ([(h++ ?pointer) (lz77-accumulative-hash heads hash (unsafe-bytes-ref window idx) shift mask)])
                       (lz77-update-hash heads prevs h++ ?pointer idx nil)
                       (insert (unsafe-idx+ idx 1) h++))])))]))

(define-syntax (define-lz77-deflate stx)
  (syntax-case stx [:]
    [(_ (lz77-deflate window codeword-select end min-match max-match farthest)
        #:args [(var : Type) ...]
        #:with [hash0 hash-size hash-shift hash-mask]
        #:chain [heads prevs]
        #:λ start body ...)
     (syntax/loc stx
       (define lz77-deflate : (->* (Bytes LZ77-Select-Codeword Type ...)
                                   (#:hash-bits Positive-Byte #:min-match Positive-Byte #:max-match Index #:farthest Index
                                    Index Index)
                                   Index)
         (lambda [#:hash-bits [hash-bits lz77-default-hash-bits]
                  #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
                  #:farthest [farthest lz77-default-farthest]
                  window codeword-select var ... [start0 0] [end (bytes-length window)]]
           (define hash-size : Index (unsafe-idxlshift 1 hash-bits))
           (define hash-shift : Index (quotient (+ hash-bits (- min-match 1)) min-match))
           (define hash-mask : Index (unsafe-idx- hash-size 1))
           (define heads : (Vectorof Index) (make-vector hash-size end))
           (define prevs : (Vectorof Index) (make-vector end end))
           (define offset : Index (unsafe-idx+ start0 min-match))
           
           (let init-hash+deflate ([hash0 : Index 0]
                                   [idx : Index start0])
             (cond [(>= idx end) (unsafe-idx- end start0)]
                   [(< idx offset)
                    (let ([code (unsafe-bytes-ref window idx)])
                      (codeword-select code idx)
                      (init-hash+deflate (lz77-accumulative-hash hash0 code hash-shift hash-mask)
                                         (unsafe-idx+ idx 1)))]
                   [else (let ([start : Index offset]) body ...)])))))]))

(define-syntax (define-lz77-inflate stx)
  (syntax-case stx []
    [(_ lz77-inflate-into #:with [unsafe-bytes-set! unsafe-bytes-copy!])
     (syntax/loc stx
       (define lz77-inflate-into : (case-> [Bytes Index Byte -> Index]
                                           [Bytes Index Index Index -> Index])
         (case-lambda
           [(dest d-idx codeword)
            (unsafe-bytes-set! dest d-idx codeword)
            (unsafe-idx+ d-idx 1)]
           [(dest d-idx distance span)
            (cond [(< d-idx distance) d-idx]
                  [else (let ([d-end (unsafe-idx+ d-idx span)])
                          (if (= distance 1)
                              (let ([codeword (unsafe-bytes-ref dest (unsafe-idx- d-idx 1))])
                                (let copy-runlength:1 ([d-pos : Index d-idx])
                                  (when (< d-pos d-end)
                                    (unsafe-bytes-set! dest d-pos codeword)
                                    (copy-runlength:1 (unsafe-idx+ d-pos 1)))))
                              (let* ([s-idx (unsafe-idx- d-idx distance)]
                                     [s-end (unsafe-idx+ s-idx span)])
                                (cond [(<= s-end d-idx) (unsafe-bytes-copy! dest d-idx dest s-idx s-end)]
                                      [else (let copy-overlap ([d-pos : Index d-idx]
                                                               [delta : Index (unsafe-idx- d-idx s-idx)])
                                              (define pos++ : Index (unsafe-idx+ d-pos delta))
                                              (if (< pos++ d-end)
                                                  (let ([2*delta (unsafe-idx+ delta delta)])
                                                    (unsafe-bytes-copy! dest d-pos dest s-idx d-pos)
                                                    (copy-overlap pos++ 2*delta))
                                                  (let ([s-end (unsafe-idx+ s-idx (unsafe-idx- d-end d-pos))])
                                                    (unsafe-bytes-copy! dest d-pos dest s-idx s-end))))])))
                          d-end)])])))]))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-deflate : (->* (Bytes LZ77-Select-Codeword ZIP-Strategy)
                            (Index Index #:hash-bits Positive-Byte #:min-match (Option Positive-Byte) #:max-match Index #:farthest Index)
                            Index)
  (lambda [#:hash-bits [hash-bits lz77-default-hash-bits]
           #:min-match [?min-match #false] #:max-match [max-match lz77-default-max-match]
           #:farthest [farthest lz77-default-farthest]
           window codeword-select strategy [start 0] [end (bytes-length window)]]
    (cond [(zip-run-strategy? strategy) ; better for PNGs
           (lz77-deflate/rle #:min-match (or ?min-match lz77-default-min-match) #:max-match max-match
                             window codeword-select strategy start end)]
          [(zip-default-strategy? strategy)
           (let* ([preference (zip-default-strategy-config strategy)]
                  [level (zip-deflation-config-level preference)]
                  [min-match (or ?min-match lz77-default-min-match)])
             (cond [(> level 3)
                    (lz77-deflate/slow #:hash-bits hash-bits #:min-match min-match #:max-match max-match #:farthest farthest
                                       window codeword-select preference start end)]
                   [(> level 0)
                    (lz77-deflate/fast #:hash-bits hash-bits #:min-match min-match #:max-match max-match #:farthest farthest
                                       window codeword-select preference start end)]
                   [else #| dead code for `zip-create` |# (lz77-deflate/no-match window codeword-select start end)]))]
          [(zip-backward-strategy? strategy) ; corresponds to the medium strategy of intel's `zlib-new`
           (let* ([preference (zip-backward-strategy-config strategy)]
                  [level (zip-deflation-config-level preference)]
                  [min-match (or ?min-match (if (< level 6) 4 lz77-default-min-match))])
             (cond [(> level 0)
                    (lz77-deflate/backward #:hash-bits hash-bits #:min-match min-match #:max-match max-match #:farthest farthest
                                           window codeword-select preference (zip-backward-strategy-insert-factor strategy) start end)]
                   [else #| dead code for `zip-create` |# (lz77-deflate/no-match window codeword-select start end)]))]
          [else ; special strategies
           (case (zip-strategy-name strategy)
             [(plain) (lz77-deflate/plain window codeword-select start end #:hash-bits hash-bits #:min-match (or ?min-match 4) #:max-match max-match)]
             [else #| huffman-only, and the fallback |# (lz77-deflate/no-match window codeword-select start end)])])))

(define lz77-inflate : (All (seed) (->* ((Sequenceof (U Byte (Pairof Index Index)))) ((Option Bytes) Index) (Values Bytes Index)))
  (lambda [in-codewords [dest #false] [d-start 0]]
    (if (not dest)
        (let-values ([(sdrowedoc total)
                      (for/fold ([codewords : (Listof (U Byte (Pairof Index Index))) null] [total : Natural 0])
                                ([cw in-codewords])
                        (values (cons cw codewords)
                                (+ total (if (byte? cw) 1 (cdr cw)))))])
          (lz77-inflate (in-list (reverse sdrowedoc)) (make-bytes total) 0))
        (values dest
                (for/fold ([total : Index d-start])
                          ([cw in-codewords])
                  (if (byte? cw)
                      (unsafe-lz77-inflate-into dest total cw)
                      (unsafe-lz77-inflate-into dest total (car cw) (cdr cw))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lz77-deflate (lz77-deflate/fast window codeword-select end min-match max-match farthest)
  #:args [(preference : ZIP-Deflation-Config)]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ start
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        [insert-limit (zip-deflation-config-lazy-limit preference)])
    (let deflate ([hash : Index hash0]
                  [m-idx : Index start]
                  [d-idx : Index start])
      (if (< m-idx end)
          (let ([d-idx++ (unsafe-idx+ d-idx 1)]
                [code (unsafe-bytes-ref window m-idx)]
                [boundary (min (unsafe-idx+ m-idx max-match) end)])
            (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash code m-idx hash-shift hash-mask end))
            (define-values (pointer span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size end))
            
            (if (< span min-match)
                (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                  (codeword-select code d-idx)
                  (deflate hash++ m-idx++ d-idx++))
                (let ([distance (unsafe-idx- m-idx pointer)])
                  (codeword-select distance span d-idx)
                  (cond [(> span insert-limit) (deflate hash++ (unsafe-idx+ m-idx span) d-idx++)]
                        [else (let ([ins-start (unsafe-idx+ m-idx 1)]
                                    [ins-end (unsafe-idx+ m-idx span)])
                                (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-shift hash-mask end) ins-end d-idx++))]))))
          d-idx))))

(define-lz77-deflate (lz77-deflate/slow window codeword-select end min-match max-match farthest)
  #:args [[preference : ZIP-Deflation-Config]]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ start
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [lazy-span (zip-deflation-config-lazy-limit preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        [invalid-span : Negative-Fixnum -1]) ; so that the previous and current spans will never coincide equalling zero
    (let deflate ([hash : Index hash0]
                  [m-idx : Index start]
                  [d-idx : Index start]
                  [p-span : (U Index Negative-Fixnum) invalid-span] ; actually it's `(U Positive-Index Negative-Fixnum)`
                  [p-distance : Index 0]
                  [p-hash : Index 0])
      (cond [(< m-idx end)
             (let ([self-code (unsafe-bytes-ref window m-idx)]
                   [boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-accumulative-hash heads hash self-code hash-shift hash-mask))
               (define-values (pointer self-span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size end))
               
               (cond [(>= p-span self-span)
                      (let* ([p-idx (unsafe-idx- m-idx 1)]
                             [p-idx++ (unsafe-idx+ p-idx p-span)])
                        (codeword-select p-distance p-span d-idx)
                        (lz77-insert-string heads prevs p-hash window p-idx p-idx++ hash-shift hash-mask end)
                        (deflate p-hash p-idx++ (unsafe-idx+ d-idx 1) invalid-span 0 hash))]
                     [(or (< self-span min-match)
                          (and (= self-span min-match) ; short matches with long distances are rarely good ideas.
                               (> (unsafe-idx- m-idx pointer) farthest)))
                      (codeword-select self-code d-idx)
                      (lz77-update-hash heads prevs hash++ ?pointer m-idx end)
                      (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1) invalid-span 0 hash)]
                     [(> self-span lazy-span)
                      (define self-idx
                        (cond [(> p-span 0)
                               (let ([p-idx (unsafe-idx- m-idx 1)])
                                 (codeword-select (unsafe-bytes-ref window p-idx) d-idx)
                                 (lz77-update-hash heads prevs p-hash p-idx end)
                                 (unsafe-idx+ d-idx 1))]
                              [else d-idx]))
                      (let ([m-idx++ (unsafe-idx+ m-idx self-span)])
                        (codeword-select (unsafe-idx- m-idx pointer) self-span self-idx)
                        (lz77-insert-string heads prevs hash++ window m-idx m-idx++ hash-shift hash-mask end)
                        (deflate hash++ m-idx++ (unsafe-idx+ self-idx 1) invalid-span 0 hash))]
                     [(> p-span 0)
                      (let ([p-idx (unsafe-idx- m-idx 1)])
                        (codeword-select (unsafe-bytes-ref window p-idx) d-idx)
                        (lz77-update-hash heads prevs p-hash p-idx end)
                        (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1) self-span (unsafe-idx- m-idx pointer) hash))]
                     [else (deflate hash++ (unsafe-idx+ m-idx 1) d-idx self-span (unsafe-idx- m-idx pointer) hash)]))]
          
            [(> p-span 0)
             (codeword-select p-distance p-span d-idx)
             (unsafe-idx+ d-idx 1)]
            
            [else d-idx]))))

(define-lz77-inflate lz77-inflate-into #:with [bytes-set! bytes-copy!])
(define-lz77-inflate unsafe-lz77-inflate-into #:with [unsafe-bytes-set! unsafe-bytes-copy!])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to the fastest strategy (in the `zlib`), which name is misleading.
(define-lz77-deflate (lz77-deflate/plain window codeword-select end min-match max-match farthest)
  #:args []
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ start
  (let deflate ([hash : Index hash0]
                [m-idx : Index start]
                [d-idx : Index start])
    (if (< m-idx end)
        (let ([d-idx++ (unsafe-idx+ d-idx 1)]
              [code (unsafe-bytes-ref window m-idx)])
          (define-values (hash++ pointer) (lz77-accumulative-hash heads hash code hash-shift hash-mask))
          (define span : Index (if (< pointer end) (lz77-backref-span window pointer m-idx (min (unsafe-idx+ m-idx max-match) end)) 0))

          ; keep the distance always closest to satisfy the huffman encoding
          (unsafe-vector*-set! heads hash++ m-idx)
          
          (if (< span min-match)
              (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                (codeword-select code d-idx)
                (deflate hash++ m-idx++ d-idx++))
              (let ([distance (unsafe-idx- m-idx pointer)])
                (codeword-select distance span d-idx)
                (deflate hash++ (unsafe-idx+ m-idx span) d-idx++))))
        d-idx)))

; corresponds to the medium strategy of intel's `zlib-new`
(define-lz77-deflate (lz77-deflate/backward window codeword-select end min-match max-match farthest)
  #:args [[preference : ZIP-Deflation-Config] [factor : Positive-Byte]]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ start
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        [insert-limit (unsafe-idx* (zip-deflation-config-lazy-limit preference) factor)])
    (let deflate ([hash : Index hash0]
                  [m-idx : Index start]
                  [d-idx : Index start]
                  [p-span : Index 0]
                  [p-distance : Index 0])
      (cond [(< m-idx end)
             (let ([self-code (unsafe-bytes-ref window m-idx)]
                   [boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash self-code m-idx hash-shift hash-mask end))
               (define-values (pointer self-span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size end))
               
               (cond [(= self-span 0)
                      (define self-idx
                        (cond [(> p-span 0)
                               (codeword-select p-distance p-span d-idx)
                               (unsafe-idx+ d-idx 1)]
                              [else d-idx]))
                      (codeword-select self-code self-idx)
                      (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ self-idx 1) 0 0)]
                     [(> p-span 0)
                      (define distance : Index (unsafe-idx- m-idx pointer)) ; despite the backward stepping, the distance remains the same
                      (define back-span : Index (lz77-backward-span window pointer m-idx p-span (unsafe-idx- max-match self-span)))
                      (define self-span++ : Index (unsafe-idx+ self-span back-span))
                      (define p-span-- : Index (unsafe-idx- p-span back-span))
                      
                      (define self-idx : Index
                        (cond [(>= p-span-- min-match)
                               (codeword-select p-distance p-span-- d-idx)
                               (unsafe-idx+ d-idx 1)]
                              [(> p-span-- 0)
                               (let select-literal ([p-pos : Index (unsafe-idx- m-idx p-span)]
                                                    [offset : Index 0])
                                 (cond [(>= offset p-span--) (unsafe-idx+ d-idx p-span--)]
                                       [else (codeword-select (unsafe-bytes-ref window p-pos) (unsafe-idx+ d-idx offset))
                                             (select-literal (unsafe-idx+ p-pos 1) (unsafe-idx+ offset 1))]))]
                              [else #| the previous match is merged |# d-idx]))
                      
                      (if (> self-span insert-limit)
                          (deflate hash++ (unsafe-idx+ m-idx self-span) self-idx self-span++ distance)
                          ; inserting string here, hashing is actually independent of the backward stepping
                          (let ([ins-start (unsafe-idx+ m-idx 1)]
                                [ins-end (unsafe-idx+ m-idx self-span)])
                            (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-shift hash-mask end)
                              ins-end self-idx self-span++ distance)))]
                     [(> self-span insert-limit) (deflate hash++ (unsafe-idx+ m-idx self-span) d-idx self-span (unsafe-idx- m-idx pointer))]
                     [else #;(> self-span 0) ; inserting string here, hashing is actually independent of the backward stepping
                      (let ([ins-start (unsafe-idx+ m-idx 1)]
                            [ins-end (unsafe-idx+ m-idx self-span)])
                        (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-shift hash-mask end)
                          ins-end d-idx self-span (unsafe-idx- m-idx pointer)))]
                     #;[else (deflate hash++ (unsafe-idx+ m-idx 1) d-idx 1 0)]))]

            [(>= p-span min-match)
             (codeword-select p-distance p-span d-idx)
             (unsafe-idx+ d-idx 1)]
            
            [(> p-span 0)
             (codeword-select (unsafe-bytes-ref window (unsafe-idx- m-idx 1)) d-idx)
             (unsafe-idx+ d-idx 1)]
            
            [else d-idx]))))

; corresponds to the run length encoding strategy (in the `zlib`)
(define lz77-deflate/rle : (->* (Bytes LZ77-Select-Codeword ZIP-Run-Strategy) (Index Index #:min-match Positive-Byte #:max-match Index) Index)
  (lambda [#:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           window codeword-select preference [start 0] [end (bytes-length window)]]
    (define distance : Positive-Byte (zip-run-strategy-length preference))
    (define offset : Index (unsafe-idx+ start (max distance min-match)))
    
    (let run-length ([idx : Index start])
      (cond [(>= idx end) (unsafe-idx- end start)]
            [(< idx offset)
             (codeword-select (unsafe-bytes-ref window idx) idx)
             (run-length (unsafe-idx+ idx 1))]
            [else (let deflate ([m-idx : Index idx]
                                [d-idx : Index idx])
                    (cond [(>= m-idx end) d-idx]
                          [else (let* ([pointer (unsafe-idx- m-idx distance)]
                                       [boundary (min (unsafe-idx+ m-idx max-match) end)]
                                       [span (lz77-backref-span window pointer m-idx boundary)])
                                  (if (< span min-match)
                                      (begin (codeword-select (unsafe-bytes-ref window m-idx) d-idx)
                                             (deflate (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1)))
                                      (begin (codeword-select distance span d-idx)
                                             (deflate (unsafe-idx+ m-idx span) (unsafe-idx+ d-idx 1)))))]))]))))

(define lz77-deflate/no-match : (->* (Bytes LZ77-Select-Codeword) (Index Index) Index)
  (lambda [window codeword-select [start 0] [end (bytes-length window)]]
    (let deflate ([m-idx : Index start]
                  [d-idx : Index 0])
      (cond [(>= m-idx end) d-idx]
            [else (codeword-select (unsafe-bytes-ref window m-idx) d-idx)
                  (deflate (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backref-span : (-> Bytes Index Index Index Index)
  (lambda [window pointer-idx match-idx stop-idx]
    (let cmp ([midx : Index match-idx]
              [pidx : Index pointer-idx])
      (cond [(>= midx stop-idx) (unsafe-idx- midx match-idx)]
            [(>= pidx match-idx) (cmp midx pointer-idx)]
            [(not (= (unsafe-bytes-ref window midx) (unsafe-bytes-ref window pidx))) (unsafe-idx- midx match-idx)]
            [else (cmp (unsafe-idx+ midx 1) (unsafe-idx+ pidx 1))]))))

(define lz77-longest-backref-span : (-> Bytes (Vectorof Index) Index Index Index Index Index Index Index (Values Index Index))
  (lambda [window chains head-pointer m-idx end good-span nice-span chain-size nil]
    (let search ([pointer : Index 0]
                 [span : Index 0]
                 [p-idx : Index head-pointer]
                 [chsize : Index chain-size])
      (cond [(or (= p-idx nil) (= chsize 0)) (values pointer span)]
            [else (let ([self-span (lz77-backref-span window p-idx m-idx end)])
                    (cond [(<= self-span span)      (search pointer span (unsafe-vector*-ref chains p-idx) (sub1 chsize))]
                          [(>= self-span good-span) (search p-idx self-span (unsafe-vector*-ref chains p-idx) (unsafe-idxrshift chsize 2))]
                          [(>= self-span nice-span) (search p-idx self-span nil 0)]
                          [else #| normal case |#   (search p-idx self-span (unsafe-vector*-ref chains p-idx) (sub1 chsize))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backward-span : (-> Bytes Index Index Index Index Index)
  (lambda [window pointer-idx match-idx prev-span limit]
    (define stop-idx : Nonnegative-Fixnum (max 0 (unsafe-fx- pointer-idx (min prev-span limit))))
    (define match-idx-1 : Index (unsafe-idx- match-idx 1))
    
    (let cmp ([midx : Index match-idx-1]
              [pidx : Fixnum (unsafe-fx- pointer-idx 1)])
      (cond [(< pidx stop-idx) (unsafe-idx- match-idx-1 midx)]
            [(not (= (unsafe-bytes-ref window midx) (unsafe-bytes-ref window pidx))) (unsafe-idx- match-idx-1 midx)]
            [else (cmp (unsafe-idx- midx 1) (unsafe-fx- pidx 1))]))))
