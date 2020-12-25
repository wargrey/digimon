#lang typed/racket/base

;;; collection//file/gunzip.rkt

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-deflation-config
  ([level : Byte]
   [good_length : Byte]  ;; reduce lazy search above this match length
   [max_lazy : Index]    ;; do not perform lazy search above this match length
   [nice_length : Index] ;; quit search above this match length
   [max_chain : Index])
  #:type-name ZIP-Deflation-Config
  #:transparent)

(define man-zip-#0-9 : (Vectorof ZIP-Deflation-Config)
  (vector (zip-deflation-config 0 0 0 0 0)           ;; store only
          (zip-deflation-config 1 4 4 8 4)           ;; maximum speed, no lazy matches
          (zip-deflation-config 2 4 5 16 8)
          (zip-deflation-config 3 4 6 32 32)
          (zip-deflation-config 4 4 4 16 16)         ;; lazy matches
          (zip-deflation-config 5 8 16 32 32)
          (zip-deflation-config 6 8 16 128 128)      ;; default
          (zip-deflation-config 7 8 32 128 256)
          (zip-deflation-config 8 32 128 258 1024)
          (zip-deflation-config 9 32 258 258 4096))) ;; maximum compression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-compression-preference : (->* (Byte) (Symbol) ZIP-Deflation-Config)
  (lambda [level [method 'deflated]]
    (vector-ref man-zip-#0-9 (if (< level 10) level 6))))
