#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bintext/crc.rkt")
(require "digitama/bintext/ipv4.rkt")

(require "digitama/unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define checksum-crc32 : (->* (Bytes) (Natural Natural) Index)
  (lambda [bs [start 0] [end (bytes-length bs)]]
    (with-asserts ([start index?]
                   [end index?])
      (unsafe-idxxor (update-crc32 (assert #xFFFFFFFF index?) bs 256 start end) #xFFFFFFFF))))

(define checksum-crc32* : (case-> [(Listof Bytes) -> Index]
                                  [Bytes Natural -> Index]
                                  [Bytes Natural Natural -> Index]
                                  [Bytes Natural Natural Natural -> Index])
  (case-lambda
    [(bs crc0 start end)
     ;;; NOTE:
     ; If the `crc0` is not a value of valid CRC32 checksum,
     ; `crc0 ^ #xFFFFFFFF` should be replaced by `~(crc0) & #xFFFFFF`.
     
     ; But the assumption above is meaningless, let's just leave it alone.
     
     (with-asserts ([start index?]
                    [end index?])
       (unsafe-idxxor (update-crc32 (unsafe-idxxor crc0 #xFFFFFFFF) bs 256 start end) #xFFFFFFFF))]

    [(bs-lst)
     (let acc ([crc0 : Index 0]
               [bs : (Listof Bytes) bs-lst])
       (cond [(null? bs) crc0]
             [else (acc (checksum-crc32* (car bs) crc0 0)
                        (cdr bs))]))]

    [(bs crc0) (checksum-crc32* bs crc0 0)]
    [(bs crc0 start) (checksum-crc32* bs crc0 start (bytes-length bs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; They're identical for the initial values being 0xFFFF or 0x0000
(define checksum-ipv4 : (->* (Bytes) (Natural Natural) Index)
  (lambda [bs [start 0] [end (bytes-length bs)]]
    (with-asserts ([start index?]
                   [end index?])
      (update-sum #xFFFF bs start end))))

(define checksum-ipv4* : (case-> [(Listof Bytes) -> Index]
                                 [Bytes Natural -> Index]
                                 [Bytes Natural Natural -> Index]
                                 [Bytes Natural Natural Natural -> Index])
  (case-lambda
    [(bs sum0 start end)
     (with-asserts ([start index?]
                    [end index?])
       (update-sum (unsafe-uint16-not sum0) bs start end))]

    [(bs-lst)
     (let acc ([sum0 : Index #xFFFF]
               [bs : (Listof Bytes) bs-lst])
       (cond [(null? bs) sum0]
             [else (acc (checksum-ipv4* (car bs) sum0 0)
                        (cdr bs))]))]

    [(bs sum0) (checksum-ipv4* bs sum0 0)]
    [(bs sum0 start) (checksum-ipv4* bs sum0 start (bytes-length bs))]))
