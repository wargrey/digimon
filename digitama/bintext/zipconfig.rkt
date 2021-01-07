#lang typed/racket/base

;;; illumos://gate/usr/src/contrib/zlib/zlib.h
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out))

(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-strategy
  ([name : Symbol]
   [flag : ZIP-Deflation-Option]) ; useless but to satisfy the ZIP File format 
  #:type-name ZIP-Strategy)

(define zip-name->maybe-strategy : (case-> ['default -> ZIP-Strategy]
                                           [Symbol -> (Option ZIP-Strategy)])
  (lambda [name]
    (case name
      [(default)                     (zip-default-preference 6)]
      [(rle RLE)                     (zip-run-preference 1)]
      [(plain fastest PLAIN FASTEST) (zip-plain-preference)]
      [(huffman-only HUFFMAN-ONLY)   (zip-huffman-only-preference)]
      [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-deflation-config
  ([level : Byte]
   [good-length : Index] ;; reduce lazy search above this match length
   [lazy-limit : Index]  ;; do not perform lazy search above this match length
   [nice-length : Index] ;; quit search above this match length, don't to be confused with `max-match`
   [chain-size : Index]) ;; quit search if already travelled the hash chain such times
  #:type-name ZIP-Deflation-Config
  #:transparent)

(struct zip-default-strategy zip-strategy
  ([config : ZIP-Deflation-Config])
  #:type-name ZIP-Default-Strategy
  #:transparent)

(struct zip-backward-strategy zip-strategy
  ([config : ZIP-Deflation-Config]
   [insert-factor : Positive-Byte]) ;; the factor of `max-lazy` for `max-insert-length`
  #:type-name ZIP-Backward-Strategy
  #:transparent)

(struct zip-run-strategy zip-strategy
  ([length : Positive-Byte])
  #:type-name ZIP-Run-Strategy
  #:transparent)

(define man-zip-#0-9 : (Vectorof ZIP-Default-Strategy)
  (vector (zip-default-strategy 'default 'normal  (zip-deflation-config 0  0   0   0    0))   ;; store only

          (zip-default-strategy 'default 'fast    (zip-deflation-config 1  4   4   8    4))   ;; maximum speed, no lazy matches
          (zip-default-strategy 'default 'normal  (zip-deflation-config 2  4   5  16    8))
          (zip-default-strategy 'default 'normal  (zip-deflation-config 3  4   6  32   32))

          (zip-default-strategy 'default 'normal  (zip-deflation-config 4  4   4  16   16))   ;; lazy matches
          (zip-default-strategy 'default 'normal  (zip-deflation-config 5  8  16  32   32))
          (zip-default-strategy 'default 'normal  (zip-deflation-config 6  8  16 128  128))   ;; default
          
          (zip-default-strategy 'default 'normal  (zip-deflation-config 7  8  32 128  256))
          (zip-default-strategy 'default 'normal  (zip-deflation-config 8 32 128 258 1024))
          (zip-default-strategy 'default 'maximum (zip-deflation-config 9 32 258 258 4096)))) ;; maximum compression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-default-preference : (-> Byte ZIP-Default-Strategy)
  (lambda [level]
    (vector-ref man-zip-#0-9 (if (< level (vector-length man-zip-#0-9)) level 6))))

; corresponds to the medium strategy of intel's `zlib-new`
(define zip-backward-preference : (->* (Byte) (Positive-Byte) ZIP-Backward-Strategy)
  (lambda [level [factor 16]]
    (define preference : ZIP-Default-Strategy (zip-default-preference level))
    (zip-backward-strategy 'backward
                           (zip-strategy-flag preference)
                           (zip-default-strategy-config preference)
                           factor)))

; corresponds to the fastest strategy (in the `zlib`), which name is misleading.
(define zip-plain-preference : (-> ZIP-Strategy)
  (lambda []
    (zip-special-preference 'plain)))

(define zip-huffman-only-preference : (-> ZIP-Strategy)
  (lambda []
    (zip-special-preference 'huffman-only)))

(define zip-run-preference : (-> Positive-Byte ZIP-Run-Strategy)
  (lambda [length]
    (zip-run-strategy 'rle 'fast length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-special-preference : (-> Symbol ZIP-Strategy)
  (let ([strategies : (HashTable Symbol ZIP-Strategy) (make-hasheq)])
    (lambda [name]
      (hash-ref! strategies name
                 (λ [] (zip-strategy name
                                     (case name
                                       [(plain) 'fast]
                                       [else 'normal])))))))

(define zip-compression-level? : (-> Any Boolean : #:+ Byte)
  (lambda [v]
    (and (byte? v)
         (< v (vector-length man-zip-#0-9)))))
