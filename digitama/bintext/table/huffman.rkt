#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/list))
(require (for-syntax racket/syntax))

(require "../../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (huffman-fixed-literal-codeword-bit-lengths stx)
  (syntax-case stx []
    [(_ ) ; NOTE: indices in [286, 287] are unused
     (with-syntax* ([(lengths ...)
                     (for/list ([idx (in-range 288)])
                       (cond [(<= idx 143) 8] ; [#b00110000,  #b10111111]
                             [(<= idx 255) 9] ; [#b110010000, #b111111111]
                             [(<= idx 279) 7] ; [#b0000000,   #b0010111]
                             [else 8]))]      ; [#b11000000,  #b11000111]
                    [maxlength (apply max (syntax->datum #'(lengths ...)))])
       (syntax/loc stx
         (values ((inst vector-immutable Byte) lengths ...)
                 maxlength)))]))

(define-syntax (huffman-fixed-distance-codeword-bit-lengths stx)
  (syntax-case stx []
    [(_ ) ; NOTE: indices in [30, 31] are unused
     (with-syntax* ([(lengths ...) (for/list ([idx (in-range 32)]) 5)]
                    [maxlength (apply max (syntax->datum #'(lengths ...)))])
       (syntax/loc stx
         (values ((inst vector-immutable Byte) lengths ...)
                 maxlength)))]))

(define-syntax (define-huffman-fixed-backref-span-literal-table stx)
  (syntax-case stx []
    [(_  huffman-backref-span-extra-bits huffman-backref-span-bases #:with span-idx0 span->symbol
         [#:tables [literal0 base-span0 extra-bits0] [literal base-span extra-bits] ...])
     (with-syntax ([(sym ...)
                    (let* ([bases (reverse (syntax->datum #'(base-span0 base-span ...)))]
                           [literals (reverse (syntax->datum #'(literal0 literal ...)))]
                           [max-span+1 (add1 (car bases))])
                      (for/fold ([syms (make-list max-span+1 0)])
                                ([span (in-range 3 max-span+1)])
                        (let search ([bs bases]
                                     [hs literals])
                          (cond [(< span (car bs)) (search (cdr bs) (cdr hs))]
                                [else (list-set syms span (car hs))]))))])
       (syntax/loc stx
         (begin (define huffman-backref-span-extra-bits : (Immutable-Vectorof Byte) (vector-immutable extra-bits0 extra-bits ...))
                (define huffman-backref-span-bases : (Immutable-Vectorof Index) (vector-immutable base-span0 base-span ...))
                (define span-idx0 : Index literal0)

                (define symbols : (Immutable-Vectorof Index) (vector-immutable sym ...))
                
                (define span->symbol : (-> Index Index)
                  (lambda [span]
                    (unsafe-vector*-ref symbols span))))))]))

(define-syntax (define-huffman-fixed-backref-distance-table stx)
  (syntax-case stx []
    [(_  huffman-distance-extra-bits huffman-distance-bases #:with distance->huffman-distance
         [#:tables [huffman-dist base-distance extra-bits] ...])
     (with-syntax ([([hi-distance ...] [lo-distance ...])
                    (let ([bases (reverse (syntax->datum #'(base-distance ...)))]
                          [huffmans (reverse (syntax->datum #'(huffman-dist ...)))])
                      (for/fold ([hi.lo (list (make-list 256 0) (make-list 256 0))])
                                ([dist-1 (in-range 32768)])
                        (let search ([bs bases]
                                     [hs huffmans])
                          (cond [(< (add1 dist-1) (car bs)) (search (cdr bs) (cdr hs))]
                                [(< dist-1 256) (list (car hi.lo) (list-set (cadr hi.lo) dist-1 (car hs)))]
                                [else (list (list-set (car hi.lo) (arithmetic-shift dist-1 -7) (car hs)) (cadr hi.lo))]))))])
       (syntax/loc stx
         (begin (define huffman-distance-extra-bits : (Immutable-Vectorof Byte) (vector-immutable extra-bits ...))
                (define huffman-distance-bases : (Immutable-Vectorof Positive-Index) (vector-immutable base-distance ...))

                (define hi-distances : (Immutable-Vectorof Byte) (vector-immutable hi-distance ...))
                (define lo-distances : (Immutable-Vectorof Byte) (vector-immutable lo-distance ...))
                
                (define distance->huffman-distance : (-> Index Byte)
                  (lambda [distance]
                    (let ([dist-1 (unsafe-idx- distance 1)])
                      (if (< dist-1 256)
                          (unsafe-vector*-ref lo-distances dist-1)
                          (unsafe-vector*-ref hi-distances (unsafe-idxrshift dist-1 7)))))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (huffman-fixed-literal-lengths huffman-fixed-literal-maxlength) (huffman-fixed-literal-codeword-bit-lengths))
(define-values (huffman-fixed-distance-lengths huffman-fixed-distance-maxlength) (huffman-fixed-distance-codeword-bit-lengths))

(define-huffman-fixed-backref-span-literal-table
  huffman-backref-extra-bits
  huffman-backref-bases
  #:with backref-span-offset backref-span->huffman-symbol
  [#:tables
   ;literal base extra
   [257     3    0]
   [258     4    0]
   [259     5    0]
   [260     6    0]
   [261     7    0]
   [262     8    0]
   [263     9    0]
   [264     10   0]
   [265     11   1]
   [266     13   1]
   [267     15   1]
   [268     17   1]
   [269     19   2]
   [270     23   2]
   [271     27   2]
   [272     31   2]
   [273     35   3]
   [274     43   3]
   [275     51   3]
   [276     59   3]
   [277     67   4]
   [278     83   4]
   [279     99   4]
   [280     115  4]
   [281     131  5]
   [282     163  5]
   [283     195  5]
   [284     227  5]

   ;; NOTE
   ; The 258 can be represented by 284, but it still deserves its own literal for shorter code
   ; since it gets used a lot in very redundant files, in which case any contents longer will be truncated.
   [285     258  0]])

(define-huffman-fixed-backref-distance-table
  huffman-distance-extra-bits
  huffman-distance-bases
  #:with backref-distance->huffman-distance
  [#:tables
   ;distance base  extra
   [0        1      0]
   [1        2      0]
   [2        3      0]
   [3        4      0]
   [4        5      1]
   [5        7      1]
   [6        9      2]
   [7        13     2]
   [8        17     3]
   [9        25     3]
   [10       33     4]
   [11       49     4]
   [12       65     5]
   [13       97     5]
   [14       129    6]
   [15       193    6]
   [16       257    7]
   [17       385    7]
   [18       513    8]
   [19       769    8]
   [20       1025   9]
   [21       1537   9]
   [22       2049  10]
   [23       3073  10]
   [24       4097  11]
   [25       6145  11]
   [26       8193  12]
   [27       12289 12]
   [28       16385 13]
   [29       24577 13]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define updistances : Index (vector-length huffman-distance-extra-bits))
