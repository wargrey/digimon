#lang typed/racket/base

(provide (all-defined-out))

(require "../unsafe/ops.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LZ77-Select-Codeword
  (case-> [Bytes Index Byte Index -> Void]
          [Bytes Index Index Index Index -> Void]))

(define-syntax (lz77-accumulative-hash stx)
  (syntax-case stx []
    [(_ hv b shift mask)
     (syntax/loc stx
       (bitwise-and mask
                    (bitwise-xor (unsafe-idxlshift hv shift)
                                 b)))]))

(define-syntax (lz77-update-hash stx)
  (syntax-case stx []
    [(_ heads hash0 b idx shift mask)
     (syntax/loc stx
       (let* ([h++ (lz77-accumulative-hash hash0 b shift mask)]
              [pointers (unsafe-vector*-ref heads h++)])
         (cond [(null? pointers) (unsafe-vector*-set! heads h++ idx) (values h++ #false)]
               [(list? pointers) (unsafe-vector*-set! heads h++ (cons idx pointers)) (values h++ pointers)]
               [else (unsafe-vector*-set! heads h++ (list idx pointers)) (values h++ pointers)])))]))

(define-syntax (define-lz77-inflate stx)
  (syntax-case stx []
    [(_ lz77-inflate-into #:with unsafe-bytes-set! unsafe-bytes-copy!)
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
(define lz77-deflate : (->* (Bytes LZ77-Select-Codeword) (#:hash-bits Positive-Byte #:min-match Positive-Byte Index Index) Index)
  (lambda [src codeword-select [start 0] [end (bytes-length src)] #:hash-bits [hash-bits 15] #:min-match [min-match 3]]
    (define min-match-1 : Byte (- min-match 1))
    (define hash-size : Index (unsafe-idxlshift 1 hash-bits))
    (define hash-shift : Index (quotient (+ hash-bits min-match-1) min-match))
    (define hash-mask : Index (unsafe-idx- hash-size 1))
    (define heads : (Vectorof (U Index (Listof Index))) (make-vector hash-size null))

    (let init-hash+deflate ([hash0 : Index 0]
                            [idx : Index start])
      (cond [(>= idx end) (unsafe-idx- end start)]
            [(>= idx min-match)
             (let deflate ([hash : Index hash0]
                           [m-idx : Index min-match]
                           [d-idx : Index min-match]
                           [?last-pointer : (Option Index) #false]
                           [last-span : Index 0])
               (if (< m-idx end)
                   (let ([d-idx++ (unsafe-idx+ d-idx 1)]
                         [code (unsafe-bytes-ref src m-idx)])
                     (define-values (h++ ?pointer) (lz77-update-hash heads hash code m-idx hash-shift hash-mask))
                     
                     (define-values (pointer span)
                       (cond [(not ?pointer) (values 0 0)]
                             [(exact-integer? ?pointer) (values ?pointer (lz77-backref-span src ?pointer m-idx))]
                             [else (lz77-longest-backref-span src ?pointer m-idx)]))
                     
                     (if (< span min-match)
                         (let ([m-idx++ (unsafe-idx+ m-idx 1)])
                           (codeword-select src m-idx code d-idx)
                           (deflate h++ m-idx++ d-idx++ #false last-span))
                         (let ([distance (unsafe-idx- m-idx pointer)])
                           (codeword-select src m-idx distance span d-idx)
                           (deflate h++ (unsafe-idx+ m-idx span) d-idx++ ?last-pointer last-span))))
                   d-idx))]
            [else ; init-hash
             (let ([code (unsafe-bytes-ref src idx)])
               (codeword-select src idx code idx)
               (init-hash+deflate (lz77-accumulative-hash hash0 code hash-shift hash-mask)
                                  (unsafe-idx+ idx 1)))]))))

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
(define-lz77-inflate lz77-inflate-into #:with bytes-set! bytes-copy!)
(define-lz77-inflate unsafe-lz77-inflate-into #:with unsafe-bytes-set! unsafe-bytes-copy!)

(define lz77-backref-span : (-> Bytes Index Index Index)
  (lambda [src pointer-idx match-idx]
    (let cmp ([midx : Index match-idx]
              [pidx : Index pointer-idx])
      (cond [(>= pidx match-idx) (cmp midx pointer-idx)]
            [(not (= (unsafe-bytes-ref src midx) (unsafe-bytes-ref src pidx))) (unsafe-idx- midx match-idx)]
            [else (cmp (unsafe-idx+ midx 1) (unsafe-idx+ pidx 1))]))))

(define lz77-longest-backref-span : (-> Bytes (Pairof Index (Listof Index)) Index (Values Index Index))
  (lambda [src p-idxes m-idx]
    (for/fold ([pointer : Index 0] [span : Index 0])
              ([p-idx : Index (in-list p-idxes)])
      (define self-span : Index (lz77-backref-span src p-idx m-idx))

      (if (> self-span span)
          (values p-idx self-span)
          (values pointer span)))))
