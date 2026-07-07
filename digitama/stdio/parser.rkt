#lang typed/racket/base

(provide (except-out (all-defined-out)))

(provide (rename-out [msb-bytes->int8  parse-int8]  [msb-bytes->int8  parse-mint8])
         (rename-out [msb-bytes->int16 parse-int16] [msb-bytes->int16 parse-mint16])
         (rename-out [msb-bytes->int32 parse-int32] [msb-bytes->int32 parse-mint32])
         (rename-out [msb-bytes->int64 parse-int64] [msb-bytes->int64 parse-mint64]))

(provide (rename-out [msb-bytes->uint8  parse-uint8]   [msb-bytes->uint8 parse-muint8])
         (rename-out [msb-bytes->uint16 parse-uint16] [msb-bytes->uint16 parse-muint16])
         (rename-out [msb-bytes->uint32 parse-uint32] [msb-bytes->uint32 parse-muint32])
         (rename-out [msb-bytes->uint64 parse-uint64] [msb-bytes->uint64 parse-muint64]))

(provide (rename-out [lsb-bytes->int8  parse-lint8]  [lsb-bytes->uint8  parse-luint8])
         (rename-out [lsb-bytes->int16 parse-lint16] [lsb-bytes->uint16 parse-luint16])
         (rename-out [lsb-bytes->int32 parse-lint32] [lsb-bytes->uint32 parse-luint32])
         (rename-out [lsb-bytes->int64 parse-lint64] [lsb-bytes->uint64 parse-luint64]))

(provide (rename-out [msb-bytes->size parse-size]
                     [msb-bytes->size parse-msize]
                     [lsb-bytes->size parse-lsize]))

(provide (rename-out [msb-bytes->float parse-float]  [msb-bytes->double parse-double]
                     [msb-bytes->float parse-mfloat] [msb-bytes->double parse-mdouble]
                     [lsb-bytes->float parse-lfloat] [lsb-bytes->double parse-ldouble]))

(require racket/unsafe/ops)

(require "../../number.rkt")

(require "../unsafe/number.rkt")
(require "../unsafe/release/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-boolean : (-> Bytes Fixnum Boolean)
  (lambda [src start]
    (> (bytes-ref src start) 0)))

(define parse-mnsize-list : (-> Bytes Fixnum Index Index (Listof Index))
  (lambda [src start size count]
    (if (> count 0)
        (let ([idxn (unsafe-idx+ (unsafe-idx* (- count 1) size) (assert start index?))])
          (let parse ([idx : Index idxn]
                      [dest : (Listof Index) null])
            (cond [(< idx start) dest]
                  [else (let ([n (msb-bytes->size src idx size)])
                          (parse (unsafe-idx- idx size)
                                 (cons n dest)))])))
        null)))
    
(define parse-nbytes : (case-> [Bytes Fixnum Index -> Bytes]
                               [Bytes Fixnum Index (-> Bytes Fixnum Fixnum Bytes) -> Bytes])
  (case-lambda
    [(src start bsize) (subbytes src start (unsafe-fx+ start bsize))]
    [(src start bsize subbytes) (subbytes src start (unsafe-fx+ start bsize))]))

(define parse-nbytes-list : (case-> [Bytes Fixnum (Listof Index) -> (Listof Bytes)]
                                    [Bytes Fixnum (Listof Index) (-> Bytes Fixnum Fixnum Bytes) -> (Listof Bytes)])
  (case-lambda
    [(src start bsizes) (parse-nbytes-list src start bsizes subbytes)]
    [(src start bsizes subbytes)
     (let parse ([last-end : Index (assert start index?)]
                 [sizes : (Listof Index) bsizes]
                 [dest : (Listof Bytes) null])
      (if (pair? sizes)
          (let ([next-end (unsafe-idx+ last-end (car sizes))])
            (parse next-end (cdr sizes)
                   (cons (subbytes src last-end next-end) dest)))
          (reverse dest)))]))

(define parse-nbytes-vector : (case-> [Bytes Fixnum (Listof Index) -> (Vectorof Bytes)]
                                      [Bytes Fixnum (Listof Index) (-> Bytes Fixnum Fixnum Bytes) -> (Vectorof Bytes)])
  (case-lambda
    [(src start bsizes) (parse-nbytes-vector src start bsizes subbytes)]
    [(src start bsizes subbytes)
     (define dest : (Vectorof Bytes) (make-vector (length bsizes) #""))
     (let parse ([last-end : Index (assert start index?)]
                 [sizes : (Listof Index) bsizes]
                 [idx : Index 0])
      (if (pair? sizes)
          (let ([next-end (unsafe-idx+ last-end (car sizes))])
            (unsafe-vector*-set! dest idx (subbytes src last-end next-end))
            (parse next-end (cdr sizes) (unsafe-idx+ idx 1)))
          dest))]))
