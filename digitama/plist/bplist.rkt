#lang typed/racket/base

;;; https://opensource.apple.com/source/CF/CF-1153.18/CFBinaryPList.c.auto.html

(provide (all-defined-out))

(require racket/file)
(require typed/racket/unsafe)

(require "../../format.rkt")
(require "../../number.rkt")

(unsafe-require/typed
 "../../number.rkt"
 [network-bytes->natural (-> Bytes Integer Integer Index)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type BPList-Stdin (U Path-String Bytes))

(struct bplist
  (; bplist
   [version : Byte]

   ; offset table
   [offset-table : (Listof Index)]
   
   ; trailer
   [sort-version : Byte]
   [offset-ref-size : Byte]
   [object-ref-size : Byte]
   [object-count : Index]
   [root-index : Index]
   [offset-table-index : Index])
  #:transparent
  #:type-name BPList)

(define bplist-unused-object-types : (Listof Byte) (list #b0111 #b1001 #b1011 #b1110 #b1111))
(define bplist-1D-types : (Listof Byte) (list #b1010 #b1100))
(define bplist-2D-types : (Listof Byte) (list #b1101))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-stdin->bytes : (-> BPList-Stdin Bytes)
  (lambda [/dev/bplin]
    (cond [(path? /dev/bplin) (file->bytes /dev/bplin)]
          [(string? /dev/bplin) (file->bytes /dev/bplin)]
          [else /dev/bplin])))

(define bplist-from-bytes : (-> Bytes BPList)
  (lambda [/dev/bplin]
    (define total : Index (bytes-length /dev/bplin))
    (define trailer : Fixnum (- total 32))

    (cond [(<= trailer 8) (bplist 0 null 0 0 0 0 0 0)]
          [else (with-asserts ([trailer index?])
                  (define-values (sversion offset-size pointer-size object-count root-index offset-table-index) (bplist-extract-trailer /dev/bplin trailer))
                  (bplist (bplist-extract-version /dev/bplin)
                          null
                          sversion offset-size pointer-size object-count root-index offset-table-index))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-pretty-hexdump : (-> Bytes Output-Port Byte Boolean Void)
  (lambda [/dev/bplin /dev/stdout offset-table-column show-unused-field?]
    (define total : Index (bytes-length /dev/bplin))
    (define trailer : Fixnum (- total 32))

    (when (> trailer 8)
      (with-asserts ([trailer index?])
        (define-values (sversion offset-size pointer-size object-count root-index offset-table-index) (bplist-extract-trailer /dev/bplin trailer))

        (for ([pos (in-range 8)])
          (bplist-print-byte /dev/bplin /dev/stdout pos (if (< pos 7) #\space #\newline) string-upcase))
        (newline /dev/stdout)

        (let print-object-table ([pos : Natural 8])
          (when (< pos offset-table-index)
            (define-values (object-type object-size) (bplist-extract-object-tag (bytes-ref /dev/bplin pos)))
            (define defined-type? : Boolean (not (memq object-type bplist-unused-object-types)))

            (unless (= pos 8)
              (newline /dev/stdout))

            (when (or defined-type? show-unused-field?)
              (bplist-print-binary-byte object-type object-size /dev/stdout))

            (cond [(and defined-type? (> object-type 0))
                   (define-values (object-real-size value-idx size-tag size-ln) (bplist-extract-object-size /dev/bplin pos object-size))

                   (when (> size-tag 0)
                     (bplist-print-binary-byte size-tag size-ln /dev/stdout)
                     (for ([pos (in-range (+ pos 2) value-idx)])
                       (bplist-print-byte /dev/bplin /dev/stdout (+ pos 2 pos) #\space string-upcase))
                     (display #\space /dev/stdout))

                   (cond [(memq object-type bplist-1D-types)
                          (for ([offset (in-range object-real-size)])
                            (bplist-print-integer-octets /dev/bplin /dev/stdout (+ value-idx (* offset pointer-size)) pointer-size #\space values))
                          (print-object-table (+ value-idx (* object-real-size pointer-size)))]
                         [(memq object-type bplist-2D-types)
                          (for ([offset (in-range (* object-real-size 2))])
                            (bplist-print-integer-octets /dev/bplin /dev/stdout (+ value-idx (* offset pointer-size)) pointer-size #\space values))
                          (print-object-table (+ value-idx (* object-real-size pointer-size 2)))]
                         [else (for ([offset (in-range object-real-size)])
                                 (bplist-print-byte /dev/bplin /dev/stdout (+ value-idx offset) #\space values))
                               (print-object-table (+ value-idx object-real-size))])]
                  [else (print-object-table (+ pos 1))])))
        (newline /dev/stdout)

        (let print-offset-table ([pos : Nonnegative-Fixnum offset-table-index]
                                 [col : Byte 1])
          (when (< pos trailer)
            (bplist-print-integer-octets /dev/bplin /dev/stdout pos offset-size (if (= col 0) #\newline #\space) string-upcase)
            (print-offset-table (+ pos offset-size) (remainder (+ col 1) offset-table-column))))
        (newline /dev/stdout)

        (unless (not show-unused-field?)
          (bplist-print-integer-octets /dev/bplin /dev/stdout trailer 5 #\space string-upcase))
        (for ([offset (in-range 5 8)])
          (bplist-print-byte /dev/bplin /dev/stdout (+ trailer offset) #\space string-upcase))
        (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer 8) 8 #\space string-upcase)
        (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer 16) 8 #\space string-upcase)
        (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer 24) 8 #\newline string-upcase)))))

(define bplist-print-binary-byte : (-> Byte Byte Output-Port Void)
  (lambda [msb lsb /dev/stdout]
    (display (~r msb #:base 2 #:min-width 4 #:pad-string "0") /dev/stdout)
    (display #\space /dev/stdout)
    (display (~r lsb #:base 2 #:min-width 4 #:pad-string "0") /dev/stdout)
    (display #\space /dev/stdout)
    (display #\space /dev/stdout)))

(define bplist-print-byte : (-> Bytes Output-Port Integer Char (-> String String) Void)
  (lambda [/dev/bplin /dev/stdout pos separator _]
    (display (_ (byte->hex-string (bytes-ref /dev/bplin pos))) /dev/stdout)
    (display separator /dev/stdout)))

(define bplist-print-integer-octets : (-> Bytes Output-Port Integer Byte Char (-> String String) Void)
  (lambda [/dev/bplin /dev/stdout pos size separator _]
    (for ([offset (in-range size)])
      (display (_ (byte->hex-string (bytes-ref /dev/bplin (+ pos offset)))) /dev/stdout))
    (display separator /dev/stdout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-extract-version : (-> Bytes Byte)
  (let ([zero (char->integer #\0)])
    (lambda [/dev/bplin]
      (assert (+ (* (- (bytes-ref /dev/bplin 6) zero) 10)
                 (- (bytes-ref /dev/bplin 7) zero))
              byte?))))

(define bplist-extract-object-tag : (-> Byte (Values Byte Byte))
  (lambda [v]
    (values (bitwise-and (arithmetic-shift v -4) #b1111)
            (bitwise-and v #b1111))))

(define bplist-extract-object-size : (-> Bytes Index Byte (Values Index Index Byte Byte))
  (lambda [/dev/bplin pos object-size]
    (define-values (object-real-size value-idx size-tag size-ln)
      (cond [(< object-size #b1111) (values object-size (+ pos 1) 0 0)]
            [else (let-values ([(size-tag size-ln) (bplist-extract-object-tag (bytes-ref /dev/bplin (+ pos 1)))])
                    (define size-size : Positive-Integer (expt 2 size-ln))
                    (define value-idx : Positive-Integer (+ pos 2 size-size))
                    (values (network-bytes->natural /dev/bplin (+ pos 2) value-idx) value-idx size-tag size-ln))]))
    (values object-real-size (assert value-idx index?) size-tag size-ln)))

(define bplist-extract-trailer : (-> Bytes Index (Values Byte Byte Byte Index Index Index))
  (lambda [/dev/bplin trailer-idx]
    (values (bytes-ref /dev/bplin (+ trailer-idx 5))
            (bytes-ref /dev/bplin (+ trailer-idx 6))
            (bytes-ref /dev/bplin (+ trailer-idx 7))
            (network-bytes->natural /dev/bplin (+ trailer-idx 8) (+ trailer-idx 16))
            (network-bytes->natural /dev/bplin (+ trailer-idx 16) (+ trailer-idx 24))
            (network-bytes->natural /dev/bplin (+ trailer-idx 24) (+ trailer-idx 32)))))
