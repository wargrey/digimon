#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://www.euccas.me/zlib
;;; https://www.intel.com/content/dam/www/public/us/en/documents/white-papers/zlib-compression-whitepaper-copy.pdf

(provide (all-defined-out))

(require "zipconfig.rkt")

(require "../unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `Index` is prior, but DrRacket might replace all `Index` with `LZ77-Symbol` in tooltips
(define-type LZ77-Symbol (U Index (Pairof Index Index)))

(define-type LZ77-Submit-Symbol
  (case-> [Index -> Void]
          [Index Index -> Void]))

(define lz77-default-safe-hash-bits : Positive-Byte 24)
(define lz77-default-hash-bits : Positive-Byte 15)
(define lz77-default-min-match : Positive-Byte 3)
(define lz77-default-max-match : Index 258)
(define lz77-default-farthest : Index 4096)
(define lz77-default-backref-span-bits : Positive-Byte 10)
(define lz77-default-filtered : Index 5) ; just for recall, client APIs must explicitly trigger the filtered strategy

;; NOTE
; The LZ77 deflation process takes a byte string as the input source,
;   and submits every single deflated symbol to the procedure `LZ77-Submit-Symbol`
;   which holds an argument as the destination index, as it suppose the destination
;   is an endless vector.
; Usually the source is large

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
    [(_ heads prevs hash0 window ins-start ins-end safer-end hash-span shift mask nil)
     (syntax/loc stx
       (let ([end (min safer-end ins-end)])
         (let insert : Index ([idx : Index ins-start]
                              [hash : Index hash0])
           (cond [(>= idx end) hash]
                 [else (let-values ([(h++ ?pointer) (lz77-accumulative-hash heads hash window idx hash-span shift mask)])
                         (lz77-update-hash heads prevs h++ ?pointer idx nil)
                         (insert (unsafe-idx+ idx 1) h++))]))))]))

(define-syntax (define-lz77-deflate/hash stx)
  (syntax-case stx [:]
    [(_ (lz77-deflate src symbol-submit start end min-match max-match farthest filtered nil)
        #:args [(var : Type) ...]
        #:with [hash0 hash-size hash-shift hash-mask]
        #:chain [heads prevs]
        #:λ reasonable-end hash-span body ...)
     (syntax/loc stx
       (define lz77-deflate : (->* (Bytes LZ77-Submit-Symbol Type ...)
                                   (#:hash-bits Positive-Index #:hash-heads (Option (Vectorof Index)) #:hash-chain (Option (Vectorof Index))
                                    #:min-match Positive-Byte #:max-match Index #:farthest Index #:filtered Index
                                    Index Index Index Boolean Index)
                                   (Values Index Index))
         (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:hash-heads [hs #false] #:hash-chain [ps #false]
                  #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
                  #:farthest [farthest lz77-default-farthest] #:filtered [filtered 0]
                  src symbol-submit var ... [start 0] [end (bytes-length src)] [prev-hash 0] [final? #true] [nil (bytes-length src)]]
           (define min-match-1 : Index (unsafe-idx- min-match 1)) ; results in higher compression ratio
           (define first-run? : Boolean (or (not hs) (eq? (unsafe-vector*-ref hs 0) 0)))
           (define offset : Index (if (not first-run?) start (unsafe-idx+ start min-match-1)))

           (cond [(>= offset end) (values (lz77-deflate/identity src symbol-submit start end) 0)]
                 [else (let* ([safe-bits (min hash-bits lz77-default-safe-hash-bits)]
                              [hash-size (unsafe-idxlshift 1 safe-bits)]
                              [hash-shift (quotient (+ safe-bits min-match-1) min-match)]
                              [hash-mask (unsafe-idx- hash-size 1)])
                         (define-values (heads prevs)
                           (values (or (and hs (and (>= (vector-length hs) hash-size)
                                                    (when first-run? (vector-fill! hs nil)) hs))
                                       ((inst make-vector Index) hash-size nil))
                                   (or (and ps (and (>= (vector-length ps) end)
                                                    (when first-run? (vector-fill! ps nil)) ps))
                                       ((inst make-vector Index) end nil))))
                         
                         (let init-hash+deflate ([hash0 : Index prev-hash]
                                                 [idx : Index start])
                           (if (< idx offset)
                               (let ([code (unsafe-bytes-ref src idx)])
                                 (init-hash+deflate (lz77-accumulative-hash hash0 code hash-shift hash-mask)
                                                    (unsafe-idx+ idx 1)))
                               (let ([reasonable-end (unsafe-idx- end min-match-1)]
                                     [hash-span min-match-1])
                                 (define-values (m-idx next-hash) (begin body ...))
                                 (values (cond [(not final?) m-idx]
                                               [else (lz77-deflate/identity src symbol-submit m-idx end)])
                                         next-hash)))))]))))]))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-deflate : (->* (Bytes LZ77-Submit-Symbol ZIP-Strategy)
                            (#:hash-bits Positive-Index #:hash-heads (Option (Vectorof Index)) #:hash-chain (Option (Vectorof Index))
                             #:min-match Positive-Byte #:max-match Index #:farthest Index #:filtered Index
                             Index Index Index Boolean Index)
                            (Values Index Index))
  (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:hash-heads [hs #false] #:hash-chain [ps #false]
           #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           #:farthest [farthest lz77-default-farthest] #:filtered [filtered 0]
           src symbol-submit strategy [start 0] [end (bytes-length src)] [hash0 0] [final? #true] [nil (bytes-length src)]]
    (cond [(zip-run-strategy? strategy) ; better for PNGs
           (values (lz77-deflate/rle src symbol-submit strategy start end #:min-match min-match #:max-match max-match) hash0)]
          [(zip-backward-strategy? strategy) ; corresponds to the medium strategy of intel's `zlib-new`
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-backward-strategy-config strategy)])
             (cond [(> level 0)
                    (lz77-deflate/backward #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                           #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                           src symbol-submit preference (zip-backward-strategy-insert-factor strategy) start end hash0 final? nil)]
                   [else #| dead code for `zip-create` |# (values (lz77-deflate/identity src symbol-submit start end) hash0)]))]
          [(zip-normal-strategy? strategy)
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-normal-strategy-config strategy)])
             (cond [(> level 0)
                    (lz77-deflate/normal #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                         #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                         src symbol-submit preference start end hash0 final? nil)]
                   [else #| dead code for `zip-create` |# (values (lz77-deflate/identity src symbol-submit start end) hash0)]))]
          [(zip-lazy-strategy? strategy)
           (let* ([level (zip-strategy-level strategy)]
                  [preference (zip-lazy-strategy-config strategy)])
             (cond [(> level 0)
                    (lz77-deflate/lazy #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                       #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                                       src symbol-submit preference start end hash0 final? nil)]
                   [else #| dead code for `zip-create` |# (values (lz77-deflate/identity src symbol-submit start end) hash0)]))]
          [else ; special strategies
           (case (zip-strategy-name strategy)
             [(plain)
              (lz77-deflate/plain #:hash-bits hash-bits #:hash-heads hs #:hash-chain ps
                                  #:min-match min-match #:max-match max-match
                                  src symbol-submit start end hash0 final? nil)]
             [else #| identity, huffman-only, and the fallback |# (values (lz77-deflate/identity src symbol-submit start end) hash0)])])))

(define lz77-deflate* : (->* (Bytes LZ77-Submit-Symbol ZIP-Strategy)
                            (#:window-bits Positive-Byte #:memory-level Positive-Byte
                             #:min-match Positive-Byte #:max-match Index #:farthest Index #:filtered Index
                             Index Index)
                            Void)
  (lambda [#:window-bits [winbits 15] #:memory-level [memlevel 8]
           #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           #:farthest [farthest lz77-default-farthest] #:filtered [filtered 0]
           src symbol-submit strategy [start 0] [end (bytes-length src)]]
    (define hash-bits : Positive-Index (+ memlevel 7))
    (define hash-size : Positive-Index (unsafe-idxlshift 1 (min hash-bits lz77-default-safe-hash-bits)))
    (define window-size/2 : Index (max (unsafe-idxlshift 1 winbits) min-match))
    (define window-size : Index (unsafe-idx* window-size/2 2))
    (define window : Bytes (make-bytes window-size 0))
    (define heads : (Vectorof Index) (make-vector hash-size 0))
    (define prevs : (Vectorof Index) (make-vector window-size/2 0))
    (define nil : Index (bytes-length src))

    (let deflate/slide ([src-idx : Index start]
                        [window-idx : Index window-size/2]
                        [prev-remained : Index 0]
                        [hash : Index 0])
      (define rest : Fixnum (- end src-idx))
      
      (cond [(> rest window-size/2)
             (let ([src-end (unsafe-idx+ src-idx window-size/2)])
               (unsafe-bytes-copy! window window-idx src src-idx src-end)
               (define-values (next-idx hash++)
                 (lz77-deflate #:hash-bits hash-bits #:hash-heads heads #:hash-chain prevs
                               #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                               window symbol-submit strategy
                               (unsafe-idx- window-idx prev-remained) (unsafe-idx+ window-idx window-size/2)
                               hash #false nil))
               
               (unsafe-bytes-copy! window 0 window window-size/2 window-size)
               (lz77-slide-hash heads prevs window-size/2 nil)
               (deflate/slide (unsafe-idx+ src-idx window-size/2) window-size/2 (unsafe-idx- window-size next-idx) hash++))]
            [(> rest 0)
             (unsafe-bytes-copy! window window-idx src src-idx end)
             (lz77-deflate #:hash-bits hash-bits #:hash-heads heads #:hash-chain prevs
                           #:min-match min-match #:max-match max-match #:farthest farthest #:filtered filtered
                           window symbol-submit strategy
                           (unsafe-idx- window-idx prev-remained) (unsafe-idx+ window-idx rest)
                           hash #true nil)
             (void)]))))

(define lz77-inflate : (->* ((Sequenceof LZ77-Symbol)) ((Option Bytes) Index #:span-bits Positive-Byte) (Values Bytes Index))
  (lambda [in-symbols [dest #false] [d-start 0] #:span-bits [span-bits lz77-default-backref-span-bits]]
    (define span-mask : Index (unsafe-idx- (unsafe-idxlshift #b1 span-bits) 1))
    
    (if (not dest)
        (let-values ([(sdrowedoc total)
                      (for/fold ([syms : (Listof LZ77-Symbol) null] [total : Natural 0])
                                ([s in-symbols])
                        (values (cons s syms)
                                (+ total (cond [(byte? s) 1]
                                               [(pair? s) (cdr s)]
                                               [else (bitwise-and s span-mask)]))))])
          (lz77-inflate (in-list (reverse sdrowedoc)) (make-bytes total) d-start #:span-bits span-bits))
        (values dest
                (for/fold ([total : Index d-start])
                          ([s in-symbols])
                  (cond [(byte? s) (lz77-inflate-into dest total s)]
                        [(pair? s) (lz77-inflate-into dest total (car s) (cdr s))]
                        [else (lz77-inflate-into dest total (unsafe-idxrshift s span-bits) (bitwise-and s span-mask))]))))))

(define lz77-backref-pair : (->* (Index Index) (Positive-Byte) Index)
  (lambda [distance span [span-bits lz77-default-backref-span-bits]]
    (bitwise-xor (unsafe-idxlshift distance lz77-default-backref-span-bits) span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to `zlib`'s fast strategy, no lazy match or filter
(define-lz77-deflate/hash (lz77-deflate/normal src symbol-submit start end min-match max-match farthest filtered nil)
  #:args [(preference : ZIP-Deflation-Config)]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]

        ; this is the "different meaning" mentioned in `zlib`
        [insert-limit (zip-deflation-config-lazy-limit preference)])
    (let deflate/normal : (Values Index Index) ([hash : Index hash0]
                                                [m-idx : Index start])
      (if (< m-idx reasonable-end)
          (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
            (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash src m-idx hash-span hash-shift hash-mask nil))
            (define-values (pointer span) (lz77-longest-backref-span src prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
            
            (if (< span min-match)
                (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                  (symbol-submit (unsafe-bytes-ref src m-idx))
                  (deflate/normal hash++ m-idx++))
                (let ([distance (unsafe-idx- m-idx pointer)])
                  (symbol-submit distance span)
                  (cond [(> span insert-limit) (deflate/normal hash++ (unsafe-idx+ m-idx span))]
                        [else (let ([ins-start (unsafe-idx+ m-idx 1)]
                                    [ins-end (unsafe-idx+ m-idx span)])
                                (deflate/normal (lz77-insert-string heads prevs hash++ src ins-start ins-end reasonable-end hash-span hash-shift hash-mask nil)
                                  ins-end))]))))
          
          (values m-idx hash)))))

; corresponds to `zlib`'s slow strategy, choose the better one between matches at current position and the next position 
(define-lz77-deflate/hash (lz77-deflate/lazy src symbol-submit start end min-match max-match farthest filtered nil)
  #:args [[preference : ZIP-Deflation-Config]]
  #:with [hash0 hash-size hash-shift hash-mask]
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
    (let deflate/lazy : (Values Index Index) ([hash : Index hash0]
                                              [m-idx : Index start]
                                              [p-span : (U #|Positive-|#Index Negative-Fixnum) invalid-span]
                                              [p-distance : Index 0]
                                              [p-hash : Index 0])
      (cond [(< m-idx reasonable-end)
             (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-accumulative-hash heads hash src m-idx hash-span hash-shift hash-mask))
               (define-values (pointer self-span) (lz77-longest-backref-span src prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
               
               (cond [(>= p-span self-span)
                      (let* ([p-idx (unsafe-idx- m-idx 1)]
                             [p-idx++ (unsafe-idx+ p-idx p-span)])
                        (symbol-submit p-distance p-span)
                        (lz77-insert-string heads prevs p-hash src p-idx p-idx++ reasonable-end hash-span hash-shift hash-mask nil)
                        (deflate/lazy p-hash p-idx++ invalid-span 0 hash))]
                     [(or (< self-span minimum/filtered-match)
                          (and (= self-span min-match) ; throw away short matches with long distances
                               (> (unsafe-idx- m-idx pointer) farthest)))
                      (symbol-submit (unsafe-bytes-ref src m-idx))
                      (lz77-update-hash heads prevs hash++ ?pointer m-idx nil)
                      (deflate/lazy hash++ (unsafe-idx+ m-idx 1) invalid-span 0 hash)]
                     [(> self-span lazy-span)
                      (when (> p-span 0)
                        (let ([p-idx (unsafe-idx- m-idx 1)])
                          (symbol-submit (unsafe-bytes-ref src p-idx))
                          (lz77-update-hash heads prevs p-hash p-idx nil)))
                      (let ([m-idx++ (unsafe-idx+ m-idx self-span)])
                        (symbol-submit (unsafe-idx- m-idx pointer) self-span)
                        (lz77-insert-string heads prevs hash++ src m-idx m-idx++ reasonable-end hash-span hash-shift hash-mask nil)
                        (deflate/lazy hash++ m-idx++ invalid-span 0 hash))]
                     [(> p-span 0)
                      (let ([p-idx (unsafe-idx- m-idx 1)])
                        (symbol-submit (unsafe-bytes-ref src p-idx))
                        (lz77-update-hash heads prevs p-hash p-idx nil)
                        (deflate/lazy hash++ (unsafe-idx+ m-idx 1) self-span (unsafe-idx- m-idx pointer) hash))]
                     [else (deflate/lazy hash++ (unsafe-idx+ m-idx 1) self-span (unsafe-idx- m-idx pointer) hash)]))]
            
            [(> p-span 0)
             (symbol-submit p-distance p-span)
             (values (unsafe-idx+ (unsafe-idx- m-idx 1) p-span) hash)]
            
            [else (values m-idx hash)]))))

(define lz77-inflate-into : (case-> [Bytes Index Index -> Index]
                                    [Bytes Index Index Index -> Index])
  (case-lambda
    [(dest d-idx sym)
     (unsafe-bytes-set! dest d-idx sym)
     (unsafe-idx+ d-idx 1)]
    [(dest d-idx distance span)
     (let ([d-end (unsafe-idx+ d-idx span)]
           [s-idx (unsafe-idx- d-idx distance)])
       (if (<= span distance)
           (unsafe-bytes-copy! dest d-idx dest s-idx (unsafe-idx+ s-idx span))
           
           (let copy-overlap ([d-pos : Index d-idx]
                              [delta : Index (unsafe-idx- d-idx s-idx)])
             (define pos++ : Index (unsafe-idx+ d-pos delta))
             (if (< pos++ d-end)
                 (let ([2*delta (unsafe-idx+ delta delta)])
                   (unsafe-bytes-copy! dest d-pos dest s-idx d-pos)
                   (copy-overlap pos++ 2*delta))
                 (let ([s-end (unsafe-idx+ s-idx (unsafe-idx- d-end d-pos))])
                   (unsafe-bytes-copy! dest d-pos dest s-idx s-end)))))
       d-end)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to `zlib`'s fastest strategy, no lazy, chain, filter or preferences
(define-lz77-deflate/hash (lz77-deflate/plain src symbol-submit start end min-match max-match farthest filtered nil)
  #:args []
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let deflate/plain : (Values Index Index) ([hash : Index hash0]
                                             [m-idx : Index start])
    (if (< m-idx reasonable-end)
        (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
          (define-values (hash++ pointer) (lz77-accumulative-hash heads hash src m-idx hash-span hash-shift hash-mask))
          (define span : Index
            (cond [(>= pointer m-idx) 0] ; `nil` is usually the length of source, which is also greater than `m-idx`
                  [else (lz77-backref-span src pointer m-idx boundary)]))
          
          ; keep the distance always closest to benefit the huffman encoding
          (lz77-update-hash heads hash++ m-idx)
          
          (if (< span min-match)
              (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                (symbol-submit (unsafe-bytes-ref src m-idx))
                (deflate/plain hash++ m-idx++))
              (let ([distance (unsafe-idx- m-idx pointer)])
                (symbol-submit distance span)
                (deflate/plain hash++ (unsafe-idx+ m-idx span)))))
        
        (values m-idx hash))))

; corresponds to the medium strategy of intel's `zlib-new`, match backward from the second one then adjust both
(define-lz77-deflate/hash (lz77-deflate/backward src symbol-submit start end min-match max-match farthest filtered nil)
  #:args [[preference : ZIP-Deflation-Config] [factor : Positive-Byte]]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:chain [heads prevs]
  #:λ reasonable-end hash-span
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-chain-size preference)]
        [insert-limit (unsafe-idx* (zip-deflation-config-lazy-limit preference) factor)])
    (let deflate/backward : (Values Index Index) ([hash : Index hash0]
                                                  [m-idx : Index start]
                                                  [p-span : Index 0]
                                                  [p-distance : Index 0])
      (cond [(< m-idx reasonable-end)
             (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
               (define-values (hash++ ?pointer) (lz77-update-hash heads prevs hash src m-idx hash-span hash-shift hash-mask nil))
               (define-values (pointer self-span) (lz77-longest-backref-span src prevs ?pointer m-idx boundary good-span nice-span chain-size nil))
               
               (cond [(or (< self-span min-match)
                          (and (= self-span min-match) ; throw away short matches with long distances
                               (> (unsafe-idx- m-idx pointer) farthest)))
                      (when (> p-span 0)
                        (symbol-submit p-distance p-span))
                      (symbol-submit (unsafe-bytes-ref src m-idx))
                      (deflate/backward hash++ (unsafe-idx+ m-idx 1) 0 0)]
                     [(> p-span 0)
                      (define distance : Index (unsafe-idx- m-idx pointer)) ; despite the backward stepping, the distance remains the same
                      (define back-span : Index (lz77-backward-span src pointer m-idx p-span (unsafe-idx- max-match self-span)))
                      (define self-span++ : Index (unsafe-idx+ self-span back-span))
                      (define p-span-- : Index (unsafe-idx- p-span back-span))
                      
                      (cond [(>= p-span-- min-match) (symbol-submit p-distance p-span--)]
                            [(> p-span-- 0) (lz77-deflate/identity src symbol-submit (unsafe-idx- m-idx p-span) (unsafe-idx- m-idx back-span))]
                            [else (void #| the previous match has been merged, see `lz77-backward-span` |#)])
                      
                      (if (> self-span insert-limit)
                          (deflate/backward hash++ (unsafe-idx+ m-idx self-span) self-span++ distance)
                          ; inserting string here, hashing is actually independent of the backward stepping
                          (let ([ins-start (unsafe-idx+ m-idx 1)]
                                [ins-end (unsafe-idx+ m-idx self-span)])
                            (deflate/backward (lz77-insert-string heads prevs hash++ src ins-start ins-end reasonable-end hash-span hash-shift hash-mask nil)
                              ins-end self-span++ distance)))]
                     [(<= self-span insert-limit) ; inserting string here, hashing is actually independent of the backward stepping
                      (let ([ins-start (unsafe-idx+ m-idx 1)]
                            [ins-end (unsafe-idx+ m-idx self-span)])
                        (deflate/backward (lz77-insert-string heads prevs hash++ src ins-start ins-end reasonable-end hash-span hash-shift hash-mask nil)
                          ins-end self-span (unsafe-idx- m-idx pointer)))]
                     [else (deflate/backward hash++ (unsafe-idx+ m-idx self-span) self-span (unsafe-idx- m-idx pointer))]))]
            
            [(> p-span 0)
             (symbol-submit p-distance p-span)

             ; NOTE: unlike the lazy one, no need to re-add the `p-span` here, as each round starts a new fresh matching process
             (values m-idx hash)]
            
            [else (values m-idx hash)]))))

; corresponds to `zlib`'s run length encoding strategy, good for PNGs
(define lz77-deflate/rle : (->* (Bytes LZ77-Submit-Symbol ZIP-Run-Strategy) (Index Index #:min-match Positive-Byte #:max-match Index) Index)
  (lambda [#:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           src symbol-submit preference [start 0] [end (bytes-length src)]]
    (define distance : Positive-Index (zip-run-strategy-length preference))
    (define offset : Index (min end (unsafe-idx+ start distance)))
    
    (let run-length ([idx : Index start])
      (cond [(< idx offset)
             (symbol-submit (unsafe-bytes-ref src idx))
             (run-length (unsafe-idx+ idx 1))]
            [else (let deflate/rle ([m-idx : Index idx])
                    (cond [(>= m-idx end) end]
                          [else (let* ([pointer (unsafe-idx- m-idx distance)]
                                       [boundary (min (unsafe-idx+ m-idx max-match) end)]
                                       [span (lz77-backref-span src pointer m-idx boundary)])
                                  (if (< span min-match)
                                      (begin (symbol-submit (unsafe-bytes-ref src m-idx))
                                             (deflate/rle (unsafe-idx+ m-idx 1)))
                                      (begin (symbol-submit distance span)
                                             (deflate/rle (unsafe-idx+ m-idx span)))))]))]))))

; corresponds to `zlib`'s huffman-only strategy, also served as the fallback and helper
(define lz77-deflate/identity : (->* (Bytes LZ77-Submit-Symbol) (Index Index) Index)
  (lambda [src symbol-submit [start 0] [end (bytes-length src)]]
    (let deflate/id ([idx : Nonnegative-Fixnum start])
      (cond [(>= idx end) end]
            [else (symbol-submit (unsafe-bytes-ref src idx))
                  (deflate/id (+ idx 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backref-span : (-> Bytes Index Index Index Index)
  (lambda [src pointer-idx match-idx stop-idx]
    (let cmp ([midx : Index match-idx]
              [pidx : Index pointer-idx])
      (cond [(>= midx stop-idx) (unsafe-idx- midx match-idx)]
            [(>= pidx match-idx) (cmp midx pointer-idx)]
            [(not (= (unsafe-bytes-ref src midx) (unsafe-bytes-ref src pidx))) (unsafe-idx- midx match-idx)]
            [else (cmp (unsafe-idx+ midx 1) (unsafe-idx+ pidx 1))]))))

(define lz77-longest-backref-span : (-> Bytes (Vectorof Index) Index Index Index Index Index Index Index (Values Index Index))
  (lambda [src chains head-pointer m-idx end good-span nice-span chain-size nil]
    (let search ([pointer : Index 0]
                 [span : Index 0]
                 [p-idx : Index head-pointer]
                 [chsize : Index chain-size])
      (cond [(or (>= p-idx m-idx) (= chsize 0)) (values pointer span)] ; `nil` is usually the length of source, which is also greater than `m-idx`
            [else (let ([self-span (lz77-backref-span src p-idx m-idx end)])
                    (cond [(<= self-span span)      (search pointer span (unsafe-vector*-ref chains p-idx) (sub1 chsize))]
                          [(>= self-span good-span) (search p-idx self-span (unsafe-vector*-ref chains p-idx) (unsafe-idxrshift chsize 2))]
                          [(>= self-span nice-span) (search p-idx self-span nil 0)]
                          [else #| normal case |#   (search p-idx self-span (unsafe-vector*-ref chains p-idx) (sub1 chsize))]))]))))

(define lz77-slide-hash : (-> (Vectorof Index) (Option (Vectorof Index)) Index Index Void)
  (lambda [heads ?prevs distance nil]
    (let ([n (vector-length heads)])
      (let slide-head ([idx : Nonnegative-Fixnum n])
        (when (< idx n)
          (define pos : Index (unsafe-vector*-ref heads idx))

          (cond [(< pos distance) (unsafe-vector*-set! heads idx nil)]
                [(< pos nil)(unsafe-vector*-set! heads idx (unsafe-idx- pos distance))])
          
          (slide-head (+ idx 1)))))

    (unless (not ?prevs)
      (let ([n (vector-length ?prevs)])
        (let slide-prev ([idx : Nonnegative-Fixnum n])
          (when (< idx n)
            (define pos : Index (unsafe-vector*-ref ?prevs idx))
            
            (cond [(< pos distance) (unsafe-vector*-set! ?prevs idx nil)]
                  [(< pos nil) (unsafe-vector*-set! ?prevs idx (unsafe-idx- pos distance))])
            
            (slide-prev (+ idx 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backward-span : (-> Bytes Index Index Index Index Index)
  (lambda [src pointer-idx match-idx prev-span limit]
    ;;; NOTE
    ; It's okay if the prev match is a subset of the current one
    ; since the prev one may be broken by a strategy, or even a bad hashing algorithm
    ; nevertheless, it's reasonable to stop before disturbing symbols that already submitted
    
    (define stop-idx : Nonnegative-Fixnum (max 0 (unsafe-fx- pointer-idx (min prev-span limit))))
    (define match-idx-1 : Index (unsafe-idx- match-idx 1))
    
    (let cmp ([midx : Index match-idx-1]
              [pidx : Fixnum (unsafe-fx- pointer-idx 1)])
      (cond [(< pidx stop-idx) (unsafe-idx- match-idx-1 midx)]
            [(not (= (unsafe-bytes-ref src midx) (unsafe-bytes-ref src pidx))) (unsafe-idx- match-idx-1 midx)]
            [else (cmp (unsafe-idx- midx 1) (unsafe-fx- pidx 1))]))))
