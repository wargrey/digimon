#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bintext/crc.rkt")
(require "digitama/unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define checksum-crc32 : (->* (Bytes) (Natural Natural) Index)
  (lambda [bs [start 0] [end (bytes-length bs)]]
    (with-asserts ([start index?]
                   [end index?])
      (unsafe-idxxor (update-crc32 (assert #xFFFFFFFF index?) bs 256 start end) #xFFFFFFFF))))

(define checksum-crc32* : (->* (Bytes) (Natural Natural Natural) Index)
  (lambda [bs [crc0 #x0] [start 0] [end (bytes-length bs)]]
    ;;; NOTE:
    ; If the `crc0` is not a value of valid CRC32 checksum,
    ; `crc0 ^ #xFFFFFFFF` should be replaced by `~(crc0) & #xFFFFFF`.

    ; But the assumption above is meaningless, let's just leave it alone.

    (with-asserts ([start index?]
                   [end index?])
      (unsafe-idxxor (update-crc32 (unsafe-idxxor crc0 #xFFFFFFFF) bs 256 start end) #xFFFFFFFF))))
