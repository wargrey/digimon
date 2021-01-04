#lang typed/racket/base

;;; illumos://gate/usr/src/contrib/zlib/zlib.h
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out))

(require "../../enumeration.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* zip-deflation-strategy #:+> ZIP-Deflation-Strategy
  deflation-strategy->index index->deflation-strategy
  [0 default
     filtered            ; more huffman less string match, better for small values randomly distributed
     huffman-only        ; no string match
     run-length-encoding ; limit the match distance to one, better for PNG
     fixed-block-only])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-deflation-config
  ([level : Byte]
   [good-length : Index] ;; reduce lazy search above this match length
   [max-lazy : Index]    ;; do not perform lazy search above this match length
   [nice-length : Index] ;; quit search above this match length
   [max-chain : Index])  ;; quit search if already travelled the hash chain so many times
  #:type-name ZIP-Deflation-Config
  #:transparent)

(define man-zip-#0-9 : (Vectorof ZIP-Deflation-Config)
  (vector (zip-deflation-config 0  0   0   0    0)   ;; store only

          (zip-deflation-config 1  4   4   8    4)   ;; maximum speed, no lazy matches
          (zip-deflation-config 2  4   5  16    8)
          (zip-deflation-config 3  4   6  32   32)

          (zip-deflation-config 4  4   4  16   16)   ;; lazy matches
          (zip-deflation-config 5  8  16  32   32)
          (zip-deflation-config 6  8  16 128  128)   ;; default
          (zip-deflation-config 7  8  32 128  256)
          (zip-deflation-config 8 32 128 258 1024)
          (zip-deflation-config 9 32 258 258 4096))) ;; maximum compression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-compression-preference : (->* (Byte) (Symbol) ZIP-Deflation-Config)
  (lambda [level [method 'deflated]]
    (vector-ref man-zip-#0-9 (if (< level 10) level 6))))
