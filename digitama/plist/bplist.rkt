#lang typed/racket/base

;;; https://opensource.apple.com/source/CF/CF-1153.18/CFBinaryPList.c.auto.html
;;; https://medium.com/@karaiskc/understanding-apples-binary-property-list-format-281e6da00dbd

(provide (all-defined-out))

(require racket/file)
(require typed/racket/unsafe)

(require "../plist.rkt")

(require "../../format.rkt")
(require "../../number.rkt")

(unsafe-require/typed
 "../../number.rkt"
 [network-bytes->natural (-> Bytes Integer Integer Index)])

(unsafe-require/typed
 racket/base
 [seconds->date (->* (Flonum) (Boolean) date)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-unused-object-types : (Listof Byte) (list #b0111 #b1001 #b1011 #b1110 #b1111))
(define bplist-1D-types : (Listof Byte) (list #b1010 #b1100))
(define bplist-2D-types : (Listof Byte) (list #b1101))

(define pblist-empty-dictionary : (Immutable-HashTable Symbol PList-Datum) (make-immutable-hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-bytes? : (-> Bytes Boolean)
  (lambda [/dev/bplin]
    (regexp-match? #rx"^bplist" /dev/bplin)))

(define bplist-list-object-table : (-> Bytes (Listof PList-Datum))
  (lambda [/dev/bplin]
    (define trailer-index : Fixnum (- (bytes-length /dev/bplin) 32))

    (cond [(not (and (bplist-bytes? /dev/bplin) (index? trailer-index))) null]
          [else (let-values ([(sversion sizeof-offset sizeof-object object-count root-offset offset-table-index) (bplist-extract-trailer /dev/bplin trailer-index)])
                  (let list-object-table ([object-pos : Natural 8]
                                          [stcejbo : (Listof PList-Datum) null])
                    (cond [(>= object-pos offset-table-index) (reverse stcejbo)]
                          [else (let-values ([(object-type object-size) (bplist-extract-byte-tag (bytes-ref /dev/bplin object-pos))])
                                  (cond [(> object-type 0)
                                         (cond [(or (= object-type #b0101) (= object-type #b0111)) ; ASCII or UTF8 String
                                                (define-values (byte-count byte-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos))
                                                (list-object-table (+ byte-count byte-pos)
                                                                   (cons (bplist-string-ref /dev/bplin byte-pos byte-count) stcejbo))]
                                               [(= object-type #b0011)
                                                (list-object-table (+ object-pos 9)
                                                                   (cons (bplist-date-ref /dev/bplin (+ object-pos 1)) stcejbo))]
                                               [(= object-type #b1101)
                                                (define-values (dict-count key-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos))
                                                #;(let extract-key-value ([kv-slot : Nonnegative-Fixnum 0]
                                                                        [dictionary : (Immutable-HashTable Symbol PList-Datum) pblist-empty-dictionary])
                                                  (if (< kv-slot dict-count)
                                                      (let ([kpos (+ key-pos (* kv-slot sizeof-object))]
                                                            [vpos (+ key-pos dict-count (* kv-slot sizeof-object))])                                   
                                                        (define-values (key value)
                                                          (values (extract-object (bplist-extract-object-position /dev/bplin kpos offset-table-index sizeof-offset trailer-index))
                                                                  (extract-object (bplist-extract-object-position /dev/bplin vpos offset-table-index sizeof-offset trailer-index))))
                                                        (extract-key-value (+ kv-slot 1)
                                                                           (cond [(string? key) (hash-set dictionary (string->symbol key) value)]
                                                                                 [else dictionary])))
                                                      dictionary))
                                                null]
                                               [else (let-values ([(val-count val-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos)])
                                                       (list-object-table (+ val-pos val-count) (cons (void) stcejbo)))])]
                                        [(= object-size #b1000) (list-object-table (+ object-pos 1) (cons #false stcejbo))]
                                        [(= object-size #b1001) (list-object-table (+ object-pos 1) (cons #true stcejbo))]
                                        [else (list-object-table (+ object-pos 1) (cons (void) stcejbo))]))])))])))

(define bplist-extract-object : (-> Bytes PList-Datum)
  (lambda [/dev/bplin]
    (define trailer-index : Fixnum (- (bytes-length /dev/bplin) 32))
    
    (when (index? trailer-index)
      (define-values (sversion sizeof-offset sizeof-object object-count root-offset offset-table-index) (bplist-extract-trailer /dev/bplin trailer-index))
      (define root-pos : Index (bplist-extract-object-position /dev/bplin root-offset offset-table-index sizeof-offset trailer-index))
      
      (let extract-object ([object-pos : Nonnegative-Fixnum root-pos])
        (when (< object-pos offset-table-index)
          (let-values ([(object-type object-size) (bplist-extract-byte-tag (bytes-ref /dev/bplin object-pos))])
            (cond [(> object-type 0)
                   (cond [(or (= object-type #b0101) (= object-type #b0111)) ; ASCII or UTF8 String
                          (define-values (byte-count byte-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos))
                          (bplist-string-ref /dev/bplin byte-pos byte-count)]
                         [(= object-type #b0011)
                          (bplist-date-ref /dev/bplin (+ object-pos 1))]
                         [(= object-type #b1101)
                          (define-values (dict-count key-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos))
                          (let extract-key-value ([kv-slot : Nonnegative-Fixnum 0]
                                                  [dictionary : (Immutable-HashTable Symbol PList-Datum) pblist-empty-dictionary])
                            (if (< kv-slot dict-count)
                                (let ([kpos (+ key-pos (* kv-slot sizeof-object))]
                                      [vpos (+ key-pos dict-count (* kv-slot sizeof-object))])                                   
                                  (define-values (key value)
                                    (values (extract-object (bplist-extract-object-position /dev/bplin kpos offset-table-index sizeof-offset trailer-index))
                                            (extract-object (bplist-extract-object-position /dev/bplin vpos offset-table-index sizeof-offset trailer-index))))
                                  (extract-key-value (+ kv-slot 1)
                                                     (cond [(string? key) (hash-set dictionary (string->symbol key) value)]
                                                           [else dictionary])))
                                dictionary))]
                         [else (void)])]
                  [(= object-size #b1000) #false]
                  [(= object-size #b1001) #true]
                  [else (void)])))))))

(define bplist-pretty-hexdump : (-> Bytes Output-Port Byte Boolean Void)
  (lambda [/dev/bplin /dev/stdout offset-table-column show-unused-field?]
    (define trailer-index : Fixnum (- (bytes-length /dev/bplin) 32))

    (when (index? trailer-index)
      (define-values (sversion sizeof-offset sizeof-object object-count root-offset offset-table-index) (bplist-extract-trailer /dev/bplin trailer-index))

      (for ([pos (in-range 8)])
        (bplist-print-byte /dev/bplin /dev/stdout pos (if (< pos 7) #\space #\newline) string-upcase))
      
      (let print-object-table ([pos : Natural 8])
        (when (< pos offset-table-index)
          (define-values (object-type object-size) (bplist-extract-byte-tag (bytes-ref /dev/bplin pos)))
          (define defined-type? : Boolean (not (memq object-type bplist-unused-object-types)))
          
          (bplist-newline /dev/stdout)
          (display (string-upcase (~r pos #:base 16 #:min-width 7 #:pad-string "0")) /dev/stdout)
          (display ": " /dev/stdout)
          
          (when (or defined-type? show-unused-field?)
            (bplist-print-binary-byte object-type object-size /dev/stdout))
          
          (cond [(and defined-type? (> object-type 0))
                 (define-values (object-real-size value-idx size-tag size-ln) (bplist-extract-object-size /dev/bplin object-size pos))
                 
                 (when (> size-tag 0)
                   (bplist-print-binary-byte size-tag size-ln /dev/stdout)
                   (for ([subpos (in-range (+ pos 2) value-idx)])
                     (bplist-print-byte /dev/bplin /dev/stdout subpos #\space string-upcase))
                   (display #\space /dev/stdout))
                 
                 (cond [(memq object-type bplist-1D-types)
                        (for ([offset (in-range object-real-size)])
                          (bplist-print-integer-octets /dev/bplin /dev/stdout (+ value-idx (* offset sizeof-object)) sizeof-object #\space values))
                        (print-object-table (+ value-idx (* object-real-size sizeof-object)))]
                       [(memq object-type bplist-2D-types)
                        (for ([offset (in-range (* object-real-size 2))])
                          (bplist-print-integer-octets /dev/bplin /dev/stdout (+ value-idx (* offset sizeof-object)) sizeof-object #\space values))
                        (print-object-table (+ value-idx (* object-real-size sizeof-object 2)))]
                       [else (for ([offset (in-range object-real-size)])
                               (bplist-print-byte /dev/bplin /dev/stdout (+ value-idx offset) #\space values))
                             (print-object-table (+ value-idx object-real-size))])]
                [else (print-object-table (+ pos 1))])))
      
      (bplist-newline /dev/stdout)
      (bplist-newline /dev/stdout)
      
      (let print-offset-table ([pos : Nonnegative-Fixnum offset-table-index]
                               [col : Byte 1])
        (when (< pos trailer-index)
          (bplist-print-integer-octets /dev/bplin /dev/stdout pos sizeof-offset (if (= col 0) #\newline #\space) string-upcase)
          (print-offset-table (+ pos sizeof-offset) (remainder (+ col 1) offset-table-column))))
      (bplist-newline /dev/stdout)
      
      (unless (not show-unused-field?)
        (bplist-print-integer-octets /dev/bplin /dev/stdout trailer-index 5 #\space string-upcase))
      (for ([offset (in-range 5 8)])
        (bplist-print-byte /dev/bplin /dev/stdout (+ trailer-index offset) #\space string-upcase))
      (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer-index 8) 8 #\space string-upcase)
      (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer-index 16) 8 #\space string-upcase)
      (bplist-print-integer-octets /dev/bplin /dev/stdout (+ trailer-index 24) 8 #\newline string-upcase)
      
      (flush-output /dev/stdout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define bplist-newline : (-> Output-Port Void)
  (lambda [/dev/stdout]
    (newline /dev/stdout)
    (flush-output /dev/stdout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-extract-version : (-> Bytes Byte)
  (let ([zero (char->integer #\0)])
    (lambda [/dev/bplin]
      (assert (+ (* (- (bytes-ref /dev/bplin 6) zero) 10)
                 (- (bytes-ref /dev/bplin 7) zero))
              byte?))))

(define bplist-extract-byte-tag : (-> Byte (Values Byte Byte))
  (lambda [v]
    (values (bitwise-and (arithmetic-shift v -4) #b1111)
            (bitwise-and v #b1111))))

(define bplist-extract-object-size : (-> Bytes Byte Index (Values Index Index Byte Byte))
  (lambda [/dev/bplin object-size pos]
    (define-values (object-real-size value-idx size-tag size-ln)
      (cond [(< object-size #b1111) (values object-size (+ pos 1) 0 0)]
            [else (let-values ([(size-tag size-ln) (bplist-extract-byte-tag (bytes-ref /dev/bplin (+ pos 1)))])
                    (define size-size : Positive-Integer (expt 2 size-ln))
                    (define value-idx : Positive-Integer (+ pos 2 size-size))
                    (values (network-bytes->natural /dev/bplin (+ pos 2) value-idx) value-idx size-tag size-ln))]))
    (values object-real-size (assert value-idx index?) size-tag size-ln)))

(define bplist-extract-object-position : (-> Bytes Nonnegative-Integer Index Byte Index Index)
  (lambda [/dev/bplin offset offset-table-index sizeof-offset trailer-index]
    (define pos : Nonnegative-Integer (+ offset-table-index (* offset sizeof-offset)))
    (cond [(>= pos trailer-index) trailer-index]
          [(= sizeof-offset 1) (bytes-ref /dev/bplin pos)]
          [else (network-bytes->natural /dev/bplin pos (+ pos sizeof-offset))])))

(define bplist-extrac-offset-table : (-> Bytes Index Byte Index (Listof Index))
  (lambda [/dev/bplin offset-table-index sizeof-offset trailer-index]
    (let extract-offset-table ([pos : Index offset-table-index]
                               [pos++ : Nonnegative-Fixnum (+ offset-table-index sizeof-offset)]
                               [stesffo : (Listof Index) null])
      (cond [(> pos++ trailer-index) (reverse stesffo)]
            [else (extract-offset-table pos++ (+ pos++ sizeof-offset)
                                        (cons (network-bytes->natural /dev/bplin pos pos++)
                                              stesffo))]))))

(define bplist-extract-trailer : (-> Bytes Index (Values Byte Byte Byte Index Index Index))
  (lambda [/dev/bplin trailer-idx]
    (values (bytes-ref /dev/bplin (+ trailer-idx 5))
            (bytes-ref /dev/bplin (+ trailer-idx 6))
            (bytes-ref /dev/bplin (+ trailer-idx 7))
            (network-bytes->natural /dev/bplin (+ trailer-idx 8) (+ trailer-idx 16))
            (network-bytes->natural /dev/bplin (+ trailer-idx 16) (+ trailer-idx 24))
            (network-bytes->natural /dev/bplin (+ trailer-idx 24) (+ trailer-idx 32)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-string-ref : (-> Bytes Index Index String)
  (lambda [/dev/bplin pos count]
    (bytes->string/utf-8 /dev/bplin #\uFFFD pos (+ pos count))))

(define bplist-date-ref : (-> Bytes Nonnegative-Fixnum date)
  (lambda [/dev/bplin pos]
    (define utc-s : Flonum (floating-point-bytes->real /dev/bplin #true pos (+ pos 8)))
    (seconds->date utc-s #false)))
