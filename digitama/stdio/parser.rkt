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

(define parse-mnsizes-list : (-> Bytes Fixnum Index Index (Listof Index))
  (lambda [src start size count]
    (define idxn (assert (unsafe-fx+ (unsafe-fx* (unsafe-fx- count 1) size) start) index?))

    (let parse ([idx : Index idxn]
                [dest : (Listof Index) null])
      (cond [(< idx start) dest]
            [else (let ([n (msb-bytes->size src idx size)])
                    (parse (unsafe-idx- idx size)
                           (cons n dest)))]))))

(define parse-nbytes : (-> Bytes Fixnum Index Bytes)
  (lambda [src start bsize]
    (subbytes src start (unsafe-fx+ start bsize))))

(define parse-nbytes-list : (-> Bytes Fixnum (Listof Index) (Listof Bytes))
  (lambda [src start bsizes]
    (for/list : (Listof Bytes) ([interval (in-list (nbytes-pairs start bsizes))])
      (subbytes src (car interval) (cdr interval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nbytes-pairs : (-> Fixnum (Listof Index) (Listof (Pairof Integer Integer)))
  (lambda [start bsizes]
    (let parse ([last-end : Index (assert start index?)]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (unsafe-idx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
