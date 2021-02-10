#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://www.euccas.me/zlib
;;; https://www.intel.com/content/dam/www/public/us/en/documents/white-papers/zlib-compression-whitepaper-copy.pdf

(provide (all-defined-out))

(require "zipconfig.rkt")

(require "../unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LZ77-Symbol (U Index (Pairof Index Index)))

(define-type LZ77-Submit-Symbol
  (case-> [Index Index -> Void]
          [Index Index Index -> Void]))

(define lz77-default-safe-hash-bits : Positive-Byte 24)
(define lz77-default-hash-bits : Positive-Byte 15)
(define lz77-default-min-match : Positive-Byte 3)
(define lz77-default-max-match : Index 258)
(define lz77-default-farthest : Index 4096)
(define lz77-default-backref-span-bits : Positive-Byte 10)
(define lz77-default-filtered : Index 5) ; just for recall, client APIs must explicitly trigger the filtered strategy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (lz77-accumulative-hash stx)
  (syntax-case stx []
    [(_ heads hv window c-idx hash-span shift mask)
     (syntax/loc stx
       (let ([h++ (lz77-accumulative-hash hv (unsafe-bytes-ref window (unsafe-idx+ c-idx hash-span)) shift mask)])
         (values h++ (unsafe-vector*-ref heads h++))))]
    [(_ hv b shift mask)
     (syntax/loc stx
       (bitwise-and mask
                    (bitwise-xor (unsafe-idxlshift hv shift)
                                 b)))]))

(define-syntax (lz77-update-hash stx)
  (syntax-case stx []
    [(_ heads prevs hash0 window idx hash-span shift mask nil)
     (syntax/loc stx
       (let-values ([(h++ ?pointer) (lz77-accumulative-hash heads hash0 window idx hash-span shift mask)])
         (lz77-update-hash heads prevs h++ ?pointer idx nil)
         (values h++ ?pointer)))]
    [(_ heads prevs hash ?pointer idx nil)
     (syntax/loc stx
       (let ()
         (lz77-update-hash heads hash idx)
         (unless (= ?pointer nil)
           (unsafe-vector*-set! prevs idx ?pointer))))]
    [(_ heads prevs hash idx nil)
     (syntax/loc stx
       (let ([?pointer (unsafe-vector*-ref heads hash)])
         (lz77-update-hash heads prevs hash ?pointer idx nil)))]
    [(_ heads hash idx)
     (syntax/loc stx
       (unsafe-vector*-set! heads hash idx))]))

(define-syntax (lz77-insert-string stx)
  (syntax-case stx []
    [(_ heads prevs hash0 window start end hash-span shift mask nil)
     (syntax/loc stx
       (let insert : Index ([idx : Index start]
                            [hash : Index hash0])
         (cond [(>= idx end) hash]
               [else (let-values ([(h++ ?pointer) (lz77-accumulative-hash heads hash window idx hash-span shift mask)])
                       (lz77-update-hash heads prevs h++ ?pointer idx nil)
                       (insert (unsafe-idx+ idx 1) h++))])))]))

(define-syntax (define-lz77-deflate/hash stx)
  (syntax-case stx [:]
    [(_ (lz77-deflate window symbol-submit end min-match max-match farthest filtered)
        #:args [(var : Type) ...]
        #:with [hash0 hash-size hash-shift hash-mask nil]
        #:chain [heads prevs]
        #:λ reasonable-end hash-span body ...)
     (syntax/loc stx
       (define lz77-deflate : (->* (Bytes LZ77-Submit-Symbol Type ...)
                                   (#:hash-bits Positive-Index #:hash-heads (Option (Vectorof Index)) #:hash-chain (Option (Vectorof Index))
                                    #:min-match Positive-Byte #:max-match Index #:farthest Index #:filtered Index
                                    Index Index)
                                   Index)
         (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:hash-heads [hs #false] #:hash-chain [ps #false]
                  #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
                  #:farthest [farthest lz77-default-farthest] #:filtered [filtered 0]
                  window symbol-submit var ... [start 0] [end (bytes-length window)]]
           (define min-match-1 : Index (unsafe-idx- min-match 1)) ; results in higher compression ratio
           (define offset : Index (unsafe-idx+ start min-match-1))

           (cond [(>= offset end) (lz77-deflate/identity window symbol-submit start end)]
                 [else (let* ([safe-bits (min hash-bits lz77-default-safe-hash-bits)]
                              [hash-size (unsafe-idxlshift 1 safe-bits)]
                              [hash-shift (quotient (+ safe-bits min-match-1) min-match)]
                              [hash-mask (unsafe-idx- hash-size 1)]
                              [nil end])
                         (define-values (heads prevs)
                           (values (or (and hs (and (>= (vector-length hs) hash-size)
                                                    (vector-fill! hs nil) hs))
                                       ((inst make-vector Index) hash-size nil))
                                   (or (and ps (and (>= (vector-length ps) end)
                                                    (vector-fill! ps nil) ps))
                                       ((inst make-vector Index) end nil))))
                         
                         (let init-hash+deflate ([hash0 : Index 0]
                                                 [idx : Index start])
                           (if (< idx offset)
                               (let ([code (unsafe-bytes-ref window idx)])
                                 (init-hash+deflate (lz77-accumulative-hash hash0 code hash-shift hash-mask)
                                                    (unsafe-idx+ idx 1)))
                               (let ([reasonable-end (unsafe-idx- end min-match-1)]
                                     [hash-span min-match-1])
                                 (define-values (m-idx d-idx) (begin body ...))
                                 (lz77-deflate/identity window symbol-submit m-idx end d-idx)))))]))))]))

(define-syntax (define-lz77-inflate stx)
  (syntax-case stx []
    [(_ lz77-inflate-into #:with [unsafe-bytes-set! unsafe-bytes-copy!])
     (syntax/loc stx
       (define lz77-inflate-into : (case-> [Bytes Index Index -> Index]
                                           [Bytes Index Index Index -> Index])
         (case-lambda
           [(dest d-idx sym)
            (unsafe-bytes-set! dest d-idx sym)
            (unsafe-idx+ d-idx 1)]
           [(dest d-idx distance span)
            (cond [(< d-idx distance) d-idx]
                  [else (let ([d-end (unsafe-idx+ d-idx span)])
                          (if (= distance 1)
                              (let ([sym (unsafe-bytes-ref dest (unsafe-idx- d-idx 1))])
                                (let copy-runlength:1 ([d-pos : Index d-idx])
                                  (when (< d-pos d-end)
                                    (unsafe-bytes-set! dest d-pos sym)
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
(define lz77-deflate : (->* (Bytes LZ77-Submit-Symbol ZIP-Strategy)
                            (#:hash-bits Positive-Index #:hash-heads (Option (Vectorof Index)) #:hash-chain (Option (Vectorof Index))
                             #:min-match (Option Positive-Byte) #:max-match Index #:farthest Index #:filtered Index
                             Index Index)
                            Index)
  (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:hash-heads [hs #false] #:hash-chain [ps #false]
           #:min-match [?min-match #false] #:max-match [max-match lz77-default-max-match]
           #:farthest [farthest lz77-default-farthest] #:filtered [filtered 0]
           window symbol-submit strategy [start 0] [end (bytes-length window)]]
    (cond [(zip-run-strategy? strategy) ; better for PNGs
           (lz77-deflate/rle #:min-match (or ?min-match lz77-default-min-match) #:max-match max-match
                             window symbol-submit strategy start end)]
          [(zip-backward-strategy? strategy) ; corresponds to the medium strategy of intel's `zlib-new`
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-backward-strategy-config strategy)]
                  [min-match (or ?min-match (if (< level 6) 4 lz77-default-min-match))])
             (cond [(> level 0)
                    (lz77-deflate/backward #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                           #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                           window symbol-submit preference (zip-backward-strategy-insert-factor strategy) start end)]
                   [else #| dead code for `zip-create` |# (lz77-deflate/identity window symbol-submit start end)]))]
          [(zip-normal-strategy? strategy)
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-normal-strategy-config strategy)]
                  [min-match (or ?min-match lz77-default-min-match)])
             (cond [(> level 0)
                    (lz77-deflate/normal #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                         #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                         window symbol-submit preference start end)]
                   [else #| dead code for `zip-create` |# (lz77-deflate/identity window symbol-submit start end)]))]
          [(zip-lazy-strategy? strategy)
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-lazy-strategy-config strategy)]
                  [min-match (or ?min-match lz77-default-min-match)])
             (cond [(> level 0)
                    (lz77-deflate/lazy #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                       #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                       window symbol-submit preference start end)]
                   [else #| dead code for `zip-create` |# (lz77-deflate/identity window symbol-submit start end)]))]
          [else ; special strategies
           (case (zip-strategy-name strategy)
             [(plain)
              (lz77-deflate/plain #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                  #:min-match (or ?min-match 4) #:max-match max-match
                                  window symbol-submit start end)]
             [else #| identity, huffman-only, and the fallback |# (lz77-deflate/identity window symbol-submit start end)])])))

(define lz77-inflate : (->* ((Sequenceof LZ77-Symbol)) ((Option Bytes) Index #:span-bits Positive-Byte) (Values Bytes Index))
  (lambda [in-symbols [dest #false] [d-start 0] #:span-bits [span-bits lz77-default-backref-span-bits]]
    (define span-mask : Index (unsafe-idx- (unsafe-idxlshift #b1 span-bits) 1))
    
    (if (not dest)
        (let-values ([(sdrowedoc total)
                      (for/fold ([syms : (Listof (U Index (Pairof Index Index))) null] [total : Natural 0])
                                ([s in-symbols])
                        (values (cons s syms)
                                (+ total (cond [(byte? s) 1]
                                               [(pair? s) (cdr s)]
                                               [else (bitwise-and s span-mask)]))))])
          (lz77-inflate (in-list (reverse sdrowedoc)) (make-bytes total) d-start #:span-bits span-bits))
        (values dest
                (for/fold ([total : Index d-start])
                          ([s in-symbols])
                  (cond [(byte? s) (unsafe-lz77-inflate-into dest total s)]
                        [(pair? s) (unsafe-lz77-inflate-into dest total (car s) (cdr s))]
                        [else (unsafe-lz77-inflate-into dest total (unsafe-idxrshift s span-bits) (bitwise-and s span-mask))]))))))

(define lz77-backref-pair : (->* (Index Index) (Positive-Byte) Index)
  (lambda [distance span [span-bits lz77-default-backref-span-bits]]
    (bitwise-xor (unsafe-idxlshift distance lz77-default-backref-span-bits) span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to `zlib`'s fast strategy, no lazy match or filter
(define-lz77-deflate/hash (lz77-deflate/normal window symbol-submit end min-match max-match farthest filtered)
  #:args [(preference : ZIP-Deflation-Config)]
  #:with [hash0 hash-size hash-shift hash-mask nil]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]

        ; this is the "different meaning" mentioned by `zlib`
        [insert-limit (zip-deflation-config-lazy-limit preference)])
    (let deflate : (Values Index Index) ([hash : Index hash0]
                                         [m-idx : Index 0]
                                         [d-idx : Index 0])
      (if (< m-idx reasonable-end)
          (let ([d-idx++ (unsafe-idx+ d-idx 1)]
                [boundary (min (unsafe-idx+ m-idx max-match) end)])
            (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash window m-idx hash-span hash-shift hash-mask nil))
            (define-values (pointer span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
            
            (if (< span min-match)
                (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                  (symbol-submit (unsafe-bytes-ref window m-idx) d-idx)
                  (deflate hash++ m-idx++ d-idx++))
                (let ([distance (unsafe-idx- m-idx pointer)])
                  (symbol-submit distance span d-idx)
                  (cond [(> span insert-limit) (deflate hash++ (unsafe-idx+ m-idx span) d-idx++)]
                        [else (let ([ins-start (unsafe-idx+ m-idx 1)]
                                    [ins-end (unsafe-idx+ m-idx span)])
                                (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-span hash-shift hash-mask nil)
                                  ins-end d-idx++))]))))
          
          (values m-idx d-idx)))))

; corresponds to `zlib`'s slow strategy, choose the better one between matches at current position and the next position 
(define-lz77-deflate/hash (lz77-deflate/lazy window symbol-submit end min-match max-match farthest filtered)
  #:args [[preference : ZIP-Deflation-Config]]
  #:with [hash0 hash-size hash-shift hash-mask nil]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [lazy-span (zip-deflation-config-lazy-limit preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        
        ; corresponds to the `zlib`'s filtered strategy, throw away small random distributed data
        [minimum/filtered-match (max min-match filtered)]

        ; so that the previous and current spans will never coincide equalling zero
        [invalid-span : Negative-Fixnum -1])
    (let deflate : (Values Index Index) ([hash : Index hash0]
                                         [m-idx : Index 0]
                                         [d-idx : Index 0]
                                         [p-span : (U #|Positive-|#Index Negative-Fixnum) invalid-span]
                                         [p-distance : Index 0]
                                         [p-hash : Index 0])
      (cond [(< m-idx reasonable-end)
             (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-accumulative-hash heads hash window m-idx hash-span hash-shift hash-mask))
               (define-values (pointer self-span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
               
               (cond [(>= p-span self-span)
                      (let* ([p-idx (unsafe-idx- m-idx 1)]
                             [p-idx++ (unsafe-idx+ p-idx p-span)])
                        (symbol-submit p-distance p-span d-idx)
                        (lz77-insert-string heads prevs p-hash window p-idx p-idx++ hash-span hash-shift hash-mask nil)
                        (deflate p-hash p-idx++ (unsafe-idx+ d-idx 1) invalid-span 0 hash))]
                     [(or (< self-span minimum/filtered-match)
                          (and (= self-span min-match) ; throw away short matches with long distances
                               (> (unsafe-idx- m-idx pointer) farthest)))
                      (symbol-submit (unsafe-bytes-ref window m-idx) d-idx)
                      (lz77-update-hash heads prevs hash++ ?pointer m-idx nil)
                      (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1) invalid-span 0 hash)]
                     [(> self-span lazy-span)
                      (define self-idx
                        (cond [(> p-span 0)
                               (let ([p-idx (unsafe-idx- m-idx 1)])
                                 (symbol-submit (unsafe-bytes-ref window p-idx) d-idx)
                                 (lz77-update-hash heads prevs p-hash p-idx nil)
                                 (unsafe-idx+ d-idx 1))]
                              [else d-idx]))
                      (let ([m-idx++ (unsafe-idx+ m-idx self-span)])
                        (symbol-submit (unsafe-idx- m-idx pointer) self-span self-idx)
                        (lz77-insert-string heads prevs hash++ window m-idx m-idx++ hash-span hash-shift hash-mask nil)
                        (deflate hash++ m-idx++ (unsafe-idx+ self-idx 1) invalid-span 0 hash))]
                     [(> p-span 0)
                      (let ([p-idx (unsafe-idx- m-idx 1)])
                        (symbol-submit (unsafe-bytes-ref window p-idx) d-idx)
                        (lz77-update-hash heads prevs p-hash p-idx nil)
                        (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1) self-span (unsafe-idx- m-idx pointer) hash))]
                     [else (deflate hash++ (unsafe-idx+ m-idx 1) d-idx self-span (unsafe-idx- m-idx pointer) hash)]))]
            
            [(> p-span 0)
             (symbol-submit p-distance p-span d-idx)
             ; NOTE: it should be the `p-idx` that added by the `p-span`, but `m-idx` isn't harmful here since we've already at the end
             (values (unsafe-idx+ m-idx p-span) (unsafe-idx+ d-idx 1))]
            
            [else (values m-idx d-idx)]))))

(define-lz77-inflate lz77-inflate-into #:with [bytes-set! bytes-copy!])
(define-lz77-inflate unsafe-lz77-inflate-into #:with [unsafe-bytes-set! unsafe-bytes-copy!])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to `zlib`'s fastest strategy, no lazy, chain, filter or preferences
(define-lz77-deflate/hash (lz77-deflate/plain window symbol-submit end min-match max-match farthest filtered)
  #:args []
  #:with [hash0 hash-size hash-shift hash-mask nil]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let deflate : (Values Index Index) ([hash : Index hash0]
                                       [m-idx : Index 0]
                                       [d-idx : Index 0])
    (if (< m-idx reasonable-end)
        (let ([d-idx++ (unsafe-idx+ d-idx 1)]
              [boundary (min (unsafe-idx+ m-idx max-match) end)])
          (define-values (hash++ pointer) (lz77-accumulative-hash heads hash window m-idx hash-span hash-shift hash-mask))
          (define span : Index (if (= pointer nil) 0 (lz77-backref-span window pointer m-idx boundary)))
          
          ; keep the distance always closest to satisfy the huffman encoding
          (lz77-update-hash heads hash++ m-idx)
          
          (if (< span min-match)
              (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                (symbol-submit (unsafe-bytes-ref window m-idx) d-idx)
                (deflate hash++ m-idx++ d-idx++))
              (let ([distance (unsafe-idx- m-idx pointer)])
                (symbol-submit distance span d-idx)
                (deflate hash++ (unsafe-idx+ m-idx span) d-idx++))))
        
        (values m-idx d-idx))))

; corresponds to the medium strategy of intel's `zlib-new`, match backward from the second one then adjust both
(define-lz77-deflate/hash (lz77-deflate/backward window symbol-submit end min-match max-match farthest filtered)
  #:args [[preference : ZIP-Deflation-Config] [factor : Positive-Byte]]
  #:with [hash0 hash-size hash-shift hash-mask nil]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        [insert-limit (unsafe-idx* (zip-deflation-config-lazy-limit preference) factor)])
    (let deflate : (Values Index Index) ([hash : Index hash0]
                                         [m-idx : Index 0]
                                         [d-idx : Index 0]
                                         [p-span : Index 0]
                                         [p-distance : Index 0])
      (cond [(< m-idx reasonable-end)
             (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash window m-idx hash-span hash-shift hash-mask nil))
               (define-values (pointer self-span) (lz77-longest-backref-span window prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
               
               (cond [(or (< self-span min-match)
                          (and (= self-span min-match) ; throw away short matches with long distances
                               (> (unsafe-idx- m-idx pointer) farthest)))
                      (define self-idx
                        (cond [(> p-span 0) (symbol-submit p-distance p-span d-idx) (unsafe-idx+ d-idx 1)]
                              [else d-idx]))
                      (symbol-submit (unsafe-bytes-ref window m-idx) self-idx)
                      (deflate hash++ (unsafe-idx+ m-idx 1) (unsafe-idx+ self-idx 1) 0 0)]
                     [(> p-span 0)
                      (define distance : Index (unsafe-idx- m-idx pointer)) ; despite the backward stepping, the distance remains the same
                      (define back-span : Index (lz77-backward-span window pointer m-idx p-span (unsafe-idx- max-match self-span)))
                      (define self-span++ : Index (unsafe-idx+ self-span back-span))
                      (define p-span-- : Index (unsafe-idx- p-span back-span))
                      
                      (define self-idx : Index
                        (cond [(>= p-span-- min-match) (symbol-submit p-distance p-span-- d-idx) (unsafe-idx+ d-idx 1)]
                              [(> p-span-- 0) (lz77-deflate/identity window symbol-submit (unsafe-idx- m-idx p-span) (unsafe-idx- m-idx back-span) d-idx)]
                              [else #| the previous match has been merged, see `lz77-backward-span` |# d-idx]))
                      
                      (if (> self-span insert-limit)
                          (deflate hash++ (unsafe-idx+ m-idx self-span) self-idx self-span++ distance)
                          ; inserting string here, hashing is actually independent of the backward stepping
                          (let ([ins-start (unsafe-idx+ m-idx 1)]
                                [ins-end (unsafe-idx+ m-idx self-span)])
                            (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-span hash-shift hash-mask nil)
                              ins-end self-idx self-span++ distance)))]
                     [(<= self-span insert-limit) ; inserting string here, hashing is actually independent of the backward stepping
                      (let ([ins-start (unsafe-idx+ m-idx 1)]
                            [ins-end (unsafe-idx+ m-idx self-span)])
                        (deflate (lz77-insert-string heads prevs hash++ window ins-start ins-end hash-span hash-shift hash-mask nil)
                          ins-end d-idx self-span (unsafe-idx- m-idx pointer)))]
                     [else (deflate hash++ (unsafe-idx+ m-idx self-span) d-idx self-span (unsafe-idx- m-idx pointer))]))]
            
            [(> p-span 0)
             (symbol-submit p-distance p-span d-idx)

             ; NOTE: unlike the lazy one, no need to re-add the `p-span` here, as each round starts a new fresh matching process
             (values m-idx (unsafe-idx+ d-idx 1))]
            
            [else (values m-idx d-idx)]))))

; corresponds to `zlib`'s run length encoding strategy, good for PNGs
(define lz77-deflate/rle : (->* (Bytes LZ77-Submit-Symbol ZIP-Run-Strategy) (Index Index #:min-match Positive-Byte #:max-match Index) Index)
  (lambda [#:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           window symbol-submit preference [start 0] [end (bytes-length window)]]
    (define distance : Positive-Byte (zip-run-strategy-length preference))
    (define offset : Index (unsafe-idx+ start (max distance min-match)))
    
    (let run-length ([idx : Index start])
      (cond [(>= idx end) (unsafe-idx- end start)]
            [(< idx offset)
             (symbol-submit (unsafe-bytes-ref window idx) idx)
             (run-length (unsafe-idx+ idx 1))]
            [else (let deflate ([m-idx : Index idx]
                                [d-idx : Index idx])
                    (cond [(>= m-idx end) d-idx]
                          [else (let* ([pointer (unsafe-idx- m-idx distance)]
                                       [boundary (min (unsafe-idx+ m-idx max-match) end)]
                                       [span (lz77-backref-span window pointer m-idx boundary)])
                                  (if (< span min-match)
                                      (begin (symbol-submit (unsafe-bytes-ref window m-idx) d-idx)
                                             (deflate (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1)))
                                      (begin (symbol-submit distance span d-idx)
                                             (deflate (unsafe-idx+ m-idx span) (unsafe-idx+ d-idx 1)))))]))]))))

; corresponds to `zlib`'s huffman-only strategy, also served as the fallback and helper
(define lz77-deflate/identity : (->* (Bytes LZ77-Submit-Symbol) (Index Index Index) Index)
  (lambda [window symbol-submit [start 0] [end (bytes-length window)] [d-idx0 0]]
    (let deflate ([m-idx : Index start]
                  [d-idx : Index d-idx0])
      (cond [(>= m-idx end) d-idx]
            [else (symbol-submit (unsafe-bytes-ref window m-idx) d-idx)
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
    ;;; NOTE
    ; It's okay if the prev match is a subset of the current one
    ; since the prev one may be broken by a strategy, or even a bad hashing algorithm
    ; nevertheless, it's reasonable to stop before disturbing symbols that already submitted
    
    (define stop-idx : Nonnegative-Fixnum (max 0 (unsafe-fx- pointer-idx (min prev-span limit))))
    (define match-idx-1 : Index (unsafe-idx- match-idx 1))
    
    (let cmp ([midx : Index match-idx-1]
              [pidx : Fixnum (unsafe-fx- pointer-idx 1)])
      (cond [(< pidx stop-idx) (unsafe-idx- match-idx-1 midx)]
            [(not (= (unsafe-bytes-ref window midx) (unsafe-bytes-ref window pidx))) (unsafe-idx- match-idx-1 midx)]
            [else (cmp (unsafe-idx- midx 1) (unsafe-fx- pidx 1))]))))
