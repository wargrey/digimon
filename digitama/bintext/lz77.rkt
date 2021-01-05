#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://www.euccas.me/zlib

(provide (all-defined-out))

(require "zipconfig.rkt")

(require "../unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LZ77-Select-Codeword
  (case-> [Bytes Index Byte Index -> Void]
          [Bytes Index Index Index Index -> Void]))

(define lz77-default-hash-bits : Positive-Byte 15)
(define lz77-default-min-match : Positive-Byte 3)
(define lz77-default-max-match : Index 258)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (lz77-accumulative-hash stx)
  (syntax-case stx []
    [(_ hv b shift mask)
     (syntax/loc stx
       (bitwise-and mask
                    (bitwise-xor (unsafe-idxlshift hv shift)
                                 b)))]))

(define-syntax (lz77-insert-string stx)
  (syntax-case stx []
    [(_ heads hash0 b idx shift mask)
     (syntax/loc stx
       (let* ([h++ (lz77-accumulative-hash hash0 b shift mask)]
              [pointers (unsafe-vector*-ref heads h++)])
         (cond [(null? pointers) (unsafe-vector*-set! heads h++ idx) (values h++ #false)]
               [(list? pointers) (unsafe-vector*-set! heads h++ (cons idx pointers)) (values h++ pointers)]
               [else (unsafe-vector*-set! heads h++ (list idx pointers)) (values h++ pointers)])))]))

(define-syntax (define-lz77-deflate stx)
  (syntax-case stx [:]
    [(_ (lz77-deflate window codeword-select end min-match max-match)
        #:args [(var : Type) ...]
        #:with [hash0 hash-size hash-shift hash-mask]
        #:head [heads #: Head-Type defvalue]
        #:λ start body ...)
     (syntax/loc stx
       (define lz77-deflate : (->* (Bytes LZ77-Select-Codeword Type ...)
                                   (Index Index #:hash-bits Positive-Byte #:min-match Positive-Byte #:max-match Index)
                                   Index)
         (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
                  window codeword-select var ... [start0 0] [end (bytes-length window)]]
           (define hash-size : Index (unsafe-idxlshift 1 hash-bits))
           (define hash-shift : Index (quotient (+ hash-bits (- min-match 1)) min-match))
           (define hash-mask : Index (unsafe-idx- hash-size 1))
           (define heads : Head-Type (make-vector hash-size defvalue))
           (define offset : Index (unsafe-idx+ start0 min-match))
           
           (let init-hash+deflate ([hash0 : Index 0]
                                   [idx : Index start0])
             (cond [(>= idx end) (unsafe-idx- end start0)]
                   [(< idx offset)
                    (let ([code (unsafe-bytes-ref window idx)])
                      (codeword-select window idx code idx)
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
                  [else (let* ([d-end (unsafe-idx+ d-idx span)]
                               [s-idx (unsafe-idx- d-idx distance)]
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
                                              (unsafe-bytes-copy! dest d-pos dest s-idx s-end))))])
                          d-end)])])))]))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-deflate : (->* (Bytes LZ77-Select-Codeword ZIP-Strategy)
                            (Index Index #:hash-bits Positive-Byte #:min-match (Option Positive-Byte) #:max-match Index)
                            Index)
  (lambda [#:hash-bits [hash-bits lz77-default-hash-bits] #:min-match [?min-match #false] #:max-match [max-match lz77-default-max-match]
           window codeword-select strategy [start 0] [end (bytes-length window)]]
    (or (and (zip-run-strategy? strategy) ; better for PNGs
             (lz77-deflate/rle #:min-match (or ?min-match lz77-default-min-match) #:max-match max-match
                               window codeword-select strategy start end))
        (and (zip-default-strategy? strategy)
             (let* ([preference (zip-default-strategy-config strategy)]
                    [level (zip-deflation-config-level preference)]
                    [min-match (or ?min-match (if (< level 6) 4 lz77-default-min-match)) #| learnt from intel zlib-new |#])
               (cond [(> level 3)
                      (lz77-deflate/slow #:hash-bits hash-bits #:min-match min-match #:max-match max-match
                                         window codeword-select preference start end)]
                     [(> level 0)
                      (lz77-deflate/fast #:hash-bits hash-bits #:min-match min-match #:max-match max-match
                                         window codeword-select preference start end)]
                     [else #| In `zip`, such a strategy will be considered as "no compression" and will not invoke `lz77-deflate` here |# #false])))
        (case (zip-strategy-name strategy)
          [(fastest) (lz77-deflate/fastest window codeword-select start end #:hash-bits hash-bits #:min-match (or ?min-match 4) #:max-match max-match)]
          [else #| 'huffman-only, and the fallback |# (lz77-deflate/no-match window codeword-select start end)]))))

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
(define-lz77-deflate (lz77-deflate/fastest window codeword-select end min-match max-match)
  #:args []
  #:with [hash0 hash-size hash-shift hash-mask]
  #:head [heads #: (Vectorof Index) end]
  #:λ start
  (let deflate ([hash : Index hash0]
                [m-idx : Index start]
                [d-idx : Index start])
    (if (< m-idx end)
        (let ([d-idx++ (unsafe-idx+ d-idx 1)]
              [code (unsafe-bytes-ref window m-idx)])
          (define h++ : Index (lz77-accumulative-hash hash code hash-shift hash-mask))
          (define pointer : Index (unsafe-vector*-ref heads h++))
          (define span : Index (if (< pointer end) (lz77-backref-span window pointer m-idx (min (unsafe-idx+ m-idx max-match) end)) 0))
          
          (unsafe-vector*-set! heads h++ m-idx)
          
          (if (< span min-match)
              (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                (codeword-select window m-idx code d-idx)
                (deflate h++ m-idx++ d-idx++))
              (let ([distance (unsafe-idx- m-idx pointer)])
                (codeword-select window m-idx distance span d-idx)
                (deflate h++ (unsafe-idx+ m-idx span) d-idx++))))
        d-idx)))

(define-lz77-deflate (lz77-deflate/fast window codeword-select end min-match max-match)
  #:args [(preference : ZIP-Deflation-Config)]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:head [heads #: (Vectorof (U Index (Listof Index))) null]
  #:λ start
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-max-chain preference)])
    (let deflate ([hash : Index hash0]
                  [m-idx : Index start]
                  [d-idx : Index start])
      (if (< m-idx end)
          (let ([d-idx++ (unsafe-idx+ d-idx 1)]
                [code (unsafe-bytes-ref window m-idx)]
                [boundary (min (unsafe-idx+ m-idx max-match) end)])
            (define-values (h++ ?pointer) (lz77-insert-string heads hash code m-idx hash-shift hash-mask))
            
            (define-values (pointer span)
              (cond [(not ?pointer) (values 0 0)]
                    [(exact-integer? ?pointer) (values ?pointer (lz77-backref-span window ?pointer m-idx boundary))]
                    [else (lz77-longest-backref-span window ?pointer m-idx boundary good-span nice-span chain-size)]))
            
            (if (< span min-match)
                (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                  (codeword-select window m-idx code d-idx)
                  (deflate h++ m-idx++ d-idx++))
                (let ([distance (unsafe-idx- m-idx pointer)])
                  (codeword-select window m-idx distance span d-idx)
                  (deflate h++ (unsafe-idx+ m-idx span) d-idx++))))
          d-idx))))

(define-lz77-deflate (lz77-deflate/slow window codeword-select end min-match max-match)
  #:args [(preference : ZIP-Deflation-Config)]
  #:with [hash0 hash-size hash-shift hash-mask]
  #:head [heads #: (Vectorof (U Index (Listof Index))) null]
  #:λ start
  (let ([good-span (zip-deflation-config-good-length preference)]
        [nice-span (zip-deflation-config-nice-length preference)]
        [chain-size (zip-deflation-config-max-chain preference)])
    (let deflate ([hash : Index hash0]
                  [m-idx : Index start]
                  [d-idx : Index start])
      (if (< m-idx end)
          (let ([d-idx++ (unsafe-idx+ d-idx 1)]
                [code (unsafe-bytes-ref window m-idx)]
                [boundary (min (unsafe-idx+ m-idx max-match) end)])
            (define-values (h++ ?pointer) (lz77-insert-string heads hash code m-idx hash-shift hash-mask))
            
            (define-values (pointer span)
              (cond [(not ?pointer) (values 0 0)]
                    [(exact-integer? ?pointer) (values ?pointer (lz77-backref-span window ?pointer m-idx boundary))]
                    [else (lz77-longest-backref-span window ?pointer m-idx boundary good-span nice-span chain-size)]))
            
            (if (< span min-match)
                (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                  (codeword-select window m-idx code d-idx)
                  (deflate h++ m-idx++ d-idx++))
                (let ([distance (unsafe-idx- m-idx pointer)])
                  (codeword-select window m-idx distance span d-idx)
                  (deflate h++ (unsafe-idx+ m-idx span) d-idx++))))
          d-idx))))

(define-lz77-inflate lz77-inflate-into #:with [bytes-set! bytes-copy!])
(define-lz77-inflate unsafe-lz77-inflate-into #:with [unsafe-bytes-set! unsafe-bytes-copy!])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-deflate/rle : (->* (Bytes LZ77-Select-Codeword ZIP-Run-Strategy) (Index Index #:min-match Positive-Byte #:max-match Index) Index)
  (lambda [#:min-match [min-match lz77-default-min-match] #:max-match [max-match lz77-default-max-match]
           window codeword-select preference [start 0] [end (bytes-length window)]]
    (define distance : Positive-Byte (zip-run-strategy-length preference))
    (define offset : Index (unsafe-idx+ start (max distance min-match)))
    
    (let run-length ([idx : Index start])
      (cond [(>= idx end) (unsafe-idx- end start)]
            [(< idx offset)
             (codeword-select window idx (unsafe-bytes-ref window idx) idx)
             (run-length (unsafe-idx+ idx 1))]
            [else (let deflate ([m-idx : Index idx]
                                [d-idx : Index idx])
                    (cond [(>= m-idx end) d-idx]
                          [else (let ([boundary (min (unsafe-idx+ m-idx max-match) end)])
                                  (define span : Index (lz77-backref-span window (unsafe-idx- m-idx distance) m-idx boundary))
                    
                                  (if (< span min-match)
                                      (begin (codeword-select window m-idx (unsafe-bytes-ref window m-idx) d-idx)
                                             (deflate (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1)))
                                      (begin (codeword-select window m-idx distance span d-idx)
                                             (deflate (unsafe-idx+ m-idx span) (unsafe-idx+ d-idx 1)))))]))]))))

(define lz77-deflate/no-match : (->* (Bytes LZ77-Select-Codeword) (Index Index) Index)
  (lambda [window codeword-select [start 0] [end (bytes-length window)]]
    (let deflate ([m-idx : Index start]
                  [d-idx : Index 0])
      (cond [(>= m-idx end) d-idx]
            [else (codeword-select window m-idx (unsafe-bytes-ref window m-idx) d-idx)
                  (deflate (unsafe-idx+ m-idx 1) (unsafe-idx+ d-idx 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backref-span : (-> Bytes Index Index Index Index)
  (lambda [window pointer-idx match-idx end]
    (let cmp ([midx : Index match-idx]
              [pidx : Index pointer-idx])
      (cond [(>= midx end) (unsafe-idx- midx match-idx)]
            [(>= pidx match-idx) (cmp midx pointer-idx)]
            [(not (= (unsafe-bytes-ref window midx) (unsafe-bytes-ref window pidx))) (unsafe-idx- midx match-idx)]
            [else (cmp (unsafe-idx+ midx 1) (unsafe-idx+ pidx 1))]))))

(define lz77-longest-backref-span : (-> Bytes (Pairof Index (Listof Index)) Index Index Index Index Index (Values Index Index))
  (lambda [window p-idxes m-idx end good-span nice-span chain-size]
    (define p-idx0 : Index (car p-idxes))
    (define span0 : Index (lz77-backref-span window p-idx0 m-idx end))
    
    (let search ([pointer : Index p-idx0]
                 [span : Index span0]
                 [idxes : (Listof Index) (cdr p-idxes)]
                 [chsize : Index chain-size])
      (cond [(or (null? idxes) (= chsize 0)) (values pointer span)]
            [else (let ([p-idx (car idxes)])
                    (define self-span : Index (lz77-backref-span window p-idx m-idx end))
                    
                    (cond [(<= self-span span)      (search pointer span (cdr idxes) (sub1 chsize))]
                          [(>= self-span good-span) (search p-idx self-span (cdr idxes) (unsafe-idxrshift chsize 2))]
                          [(>= self-span nice-span) (search p-idx self-span null 0)]
                          [else #| normal case |#   (search p-idx self-span (cdr idxes) (sub1 chsize))]))]))))
