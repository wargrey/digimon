#lang typed/racket/base

;;; illumos://gate/usr/src/contrib/zlib/zlib.h
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out))

(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-strategy
  ([name : Symbol]
   
    ; useless but to satisfy the ZIP File format
   [flag : ZIP-Deflation-Option]

   ; the compression levels are defined by `zlib`, and re-configured by `zlib-new`
   ; #0 for stored block, #1 for fastest of a concrete strategy, #9 for ratio-highest
   [level : Index])
  #:type-name ZIP-Strategy)

; TODO: find the proper strategies so that fastest for #1 and ratio-highest for #9
(define zip-level->maybe-strategy : (-> Index (Option ZIP-Strategy))
  (lambda [level]
    (case level
      [(1 2 3) (zip-normal-preference level)]
      [(4 5 6) (zip-backward-preference level)]
      [(7 8 9) (zip-lazy-preference level)]
      [(0)     (zip-identity-preference)]
      [else #false])))

(define zip-name->maybe-strategy : (-> Symbol (Option ZIP-Strategy))
  (lambda [name]
    (case name
      [(normal fast)           (zip-normal-preference 1)]
      [(lazy slow)             (zip-lazy-preference 6)]
      [(default backward)      (zip-default-preference)]
      [(rle run)               (zip-run-preference 1)]
      [(plain fastest)         (zip-plain-preference)]
      [(identity huffman-only) (zip-identity-preference)]
      [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-deflation-config
  ([good-length : Index] ;; reduce lazy search above this match length
   [lazy-limit : Index]  ;; do not perform lazy search above this match length
   [nice-length : Index] ;; quit search above this match length, don't to be confused with `max-match`
   [chain-size : Index]) ;; quit search if already travelled the hash chain such times
  #:type-name ZIP-Deflation-Config
  #:transparent)

(struct zip-normal-strategy zip-strategy
  ([config : ZIP-Deflation-Config])
  #:type-name ZIP-Normal-Strategy
  #:transparent)

(struct zip-lazy-strategy zip-strategy
  ([config : ZIP-Deflation-Config])
  #:type-name ZIP-Lazy-Strategy
  #:transparent)

(struct zip-backward-strategy zip-strategy
  ([config : ZIP-Deflation-Config]
   [insert-factor : Positive-Byte])
  #:type-name ZIP-Backward-Strategy
  #:transparent)

(struct zip-run-strategy zip-strategy
  ([length : Positive-Index])
  #:type-name ZIP-Run-Strategy
  #:transparent)

(define man-zip-#0-9 : (Vectorof (Pairof ZIP-Deflation-Option ZIP-Deflation-Config))
  (vector (cons 'normal  (zip-deflation-config  0   0   0    0))   ;; store only

          (cons 'fast    (zip-deflation-config  4   4   8    4))   ;; maximum speed
          (cons 'normal  (zip-deflation-config  4   5  16    8))
          (cons 'normal  (zip-deflation-config  4   6  32   32))

          (cons 'normal  (zip-deflation-config  4   4  16   16))
          (cons 'normal  (zip-deflation-config  8  16  32   32))
          (cons 'normal  (zip-deflation-config  8  16 128  128))   ;; default
          
          (cons 'normal  (zip-deflation-config  8  32 128  256))
          (cons 'normal  (zip-deflation-config 32 128 258 1024))
          (cons 'maximum (zip-deflation-config 32 258 258 4096)))) ;; maximum compression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; corresponds to the fastest strategy (of `zlib`)
(define zip-plain-preference : (-> ZIP-Strategy)
  (lambda []
    (zip-special-preference 'plain)))

; corresponds to the fast strategy (of `zlib`)
(define zip-normal-preference : (-> Index ZIP-Normal-Strategy)
  (lambda [level]
    (let ([preference (zip-default-configuration level)])
      (zip-normal-strategy 'normal (cadr preference) (car preference) (cddr preference)))))

; corresponds to the slow strategy (of `zlib`)
(define zip-lazy-preference : (-> Index ZIP-Lazy-Strategy)
  (lambda [level]
    (let ([preference (zip-default-configuration level)])
      (zip-lazy-strategy 'lazy (cadr preference) (car preference) (cddr preference)))))

; corresponds to the medium strategy of intel's `zlib-new`
(define zip-backward-preference : (->* (Index) (Positive-Byte) ZIP-Backward-Strategy)
  (lambda [level [factor 16]]
    (let ([preference (zip-default-configuration level)])
      (zip-backward-strategy 'backward (cadr preference) (car preference) (cddr preference) factor))))

(define zip-identity-preference : (-> ZIP-Strategy)
  (lambda []
    (zip-special-preference 'identity)))

(define zip-run-preference : (-> Positive-Index ZIP-Run-Strategy)
  (lambda [length]
    (zip-run-strategy 'rle 'fast 1 length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-default-preference : (-> ZIP-Backward-Strategy)
  (lambda []
    (zip-backward-preference 6)))

(define zip-special-preference : (-> Symbol ZIP-Strategy)
  (let ([strategies : (HashTable Symbol ZIP-Strategy) (make-hasheq)])
    (lambda [name]
      (hash-ref! strategies name
                 (λ [] (zip-strategy name
                                     (case name
                                       [(plain) 'fast]
                                       [else 'normal])
                                     1))))))

(define zip-default-configuration : (-> Index (Pairof Index (Pairof ZIP-Deflation-Option ZIP-Deflation-Config)))
  (lambda [level0]
    (define level : Index (if (< level0 (vector-length man-zip-#0-9)) level0 6))
    (cons level (vector-ref man-zip-#0-9 level))))

(define zip-compression-level? : (-> Any Boolean : #:+ Byte)
  (lambda [v]
    (and (byte? v)
         (< v (vector-length man-zip-#0-9)))))
