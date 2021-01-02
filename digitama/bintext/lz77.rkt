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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-deflate : (->* (Bytes LZ77-Select-Codeword) (#:hash-bits Positive-Byte #:min-match Positive-Byte Index Index) Index)
  (lambda [src codeword-select [start 0] [end (bytes-length src)] #:hash-bits [hash-bits 15] #:min-match [min-match 3]]
    (define min-match-1 : Byte (- min-match 1))
    (define hash-size : Index (unsafe-idxlshift 1 hash-bits))
    (define hash-shift : Byte (remainder (+ hash-bits min-match-1) min-match))
    (define hash-mask : Index (unsafe-idx- hash-size 1))
    (define heads : (Vectorof (U Index (Listof Index))) (make-vector hash-size null))

    (let init-hash+deflate ([hash0 : Index 0]
                            [idx : Index start])
      (cond [(>= idx end) (unsafe-idx- end start)]
            [(>= idx min-match)
             (let deflate ([hash : Index hash0]
                           [m-idx : Index min-match]
                           [d-idx : Index min-match])
               (if (>= m-idx end)
                   d-idx
                   (let*-values ([(b) (unsafe-bytes-ref src m-idx)]
                                 [(s-idx) (unsafe-idx- m-idx min-match)]
                                 [(h++ ?pointer) (lz77-update-hash heads hash b s-idx hash-shift hash-mask)])
                     (define idx++ : Index
                       (cond [(not ?pointer)
                              (let ([code (unsafe-bytes-ref src m-idx)])
                                (codeword-select src m-idx code d-idx)
                                (unsafe-idx+ m-idx 1))]
                             [(exact-integer? ?pointer)
                              (let ([brend (lz77-backref-span src ?pointer s-idx)])
                                (codeword-select src m-idx (unsafe-idx- s-idx ?pointer) (unsafe-idx- brend s-idx) d-idx)
                                brend)]
                             [else
                              (let-values ([(pointer brend) (lz77-longest-backref-span src ?pointer s-idx)])
                                (codeword-select src m-idx (unsafe-idx- s-idx pointer) (unsafe-idx- brend s-idx) d-idx)
                                brend)]))
                     (deflate h++ idx++ (unsafe-idx+ d-idx 1)))))]
            [else ; init-hash
             (let ([code (unsafe-bytes-ref src idx)])
               (codeword-select src idx code idx)
               (init-hash+deflate (lz77-accumulative-hash hash0 code hash-shift hash-mask)
                                  (unsafe-idx+ idx 1)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-backref-span : (-> Bytes Index Index Index)
  (lambda [src p-idx c-idx]
    (let cmp ([cidx : Index c-idx]
              [pidx : Index p-idx])
      (cond [(>= pidx c-idx) cidx]
            [(not (= (unsafe-bytes-ref src cidx) (unsafe-bytes-ref src pidx))) cidx]
            [else (cmp (unsafe-idx+ cidx 1) (unsafe-idx+ pidx 1))]))))

(define lz77-longest-backref-span : (-> Bytes (Pairof Index (Listof Index)) Index (Values Index Index))
  (lambda [src p-idxes c-idx]
    (for/fold ([pointer : Index 0] [length : Index 0])
              ([p-idx : Index (in-list p-idxes)])
      (define self-length : Index (lz77-backref-span src p-idx c-idx))

      (if (> self-length length)
          (values p-idx self-length)
          (values pointer length)))))
