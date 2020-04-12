#lang typed/racket/base

;;; https://opensource.apple.com/source/CF/CF-1153.18/CFBinaryPList.c.auto.html
;;; https://medium.com/@karaiskc/understanding-apples-binary-property-list-format-281e6da00dbd

(provide (all-defined-out))

(require typed/racket/unsafe)

(require racket/port)

(require "../plist.rkt")

(require "../../format.rkt")
(require "../../number.rkt")

(unsafe-require/typed
 "../../number.rkt"
 [network-bytes->natural (-> Bytes Integer Integer Index)])

(unsafe-require/typed
 racket/base
 [seconds->date (->* (Flonum) (Boolean) date)])

(unsafe-require/typed
 racket/date
 [date*->seconds (->* (date) (Boolean) Real)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type BPList-Unique-Objects (Immutable-HashTable Any Natural))

(define bplist-unused-object-types : (Listof Byte) (list #b0111 #b1001 #b1011 #b1110 #b1111))
(define bplist-power-types : (Listof Byte) (list #b0001 #b0010 #b0011))
(define bplist-1D-types : (Listof Byte) (list #b1010 #b1100))
(define bplist-2D-types : (Listof Byte) (list #b1101))

(define pblist-empty-unqiues : BPList-Unique-Objects (make-immutable-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-bytes? : (-> Bytes Boolean)
  (lambda [/dev/bplin]
    (regexp-match? #rx"^bplist" /dev/bplin)))

(define bplist-extract-object : (-> Bytes PList-Datum)
  (lambda [/dev/bplin]
    (define trailer-index : Fixnum (- (bytes-length /dev/bplin) 32))
    
    (when (index? trailer-index)
      (define-values (sizeof-offset sizeof-object object-count root-offset offset-table-index) (bplist-extract-trailer /dev/bplin trailer-index))
      (define root-pos : Index (bplist-extract-object-position /dev/bplin root-offset offset-table-index sizeof-offset trailer-index))
      
      (let extract-object ([object-pos : Nonnegative-Fixnum root-pos])
        (when (< object-pos offset-table-index)
          (let-values ([(object-type object-size) (bplist-extract-byte-tag (bytes-ref /dev/bplin object-pos))])
            (cond [(> object-type 0)
                   (cond [(or (= object-type #b0101) (= object-type #b0111)) ; ASCII or UTF8 String
                          (define-values (byte-count byte-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #false))
                          (bplist-string-ref /dev/bplin byte-pos byte-count)]
                         [(= object-type #b0001)
                          (define-values (integer-byte-size integer-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #true))
                          (bplist-integer-ref /dev/bplin integer-pos integer-byte-size)]
                         [(= object-type #b0010)
                          (define-values (flonum-byte-size flonum-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #true))
                          (bplist-real-ref /dev/bplin flonum-pos flonum-byte-size)]
                         [(= object-type #b0011)
                          (bplist-date-ref /dev/bplin (+ object-pos 1))]
                         [(= object-type #b1010)
                          (define-values (array-count idx-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #false))
                          (build-vector array-count
                                        (lambda [[idx-slot : Index]]
                                          (let ([ioffset (bplist-extract-object-offset /dev/bplin (+ idx-pos (* idx-slot sizeof-object)) sizeof-object)])
                                            (extract-object (bplist-extract-object-position /dev/bplin ioffset offset-table-index sizeof-offset trailer-index)))))]
                         [(= object-type #b1101)
                          (define-values (dict-count key-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #false))
                          (define dictionary : (HashTable Symbol PList-Datum) (make-hasheq))
                          (for ([kv-slot (in-range dict-count)])
                            (let ([koffset (bplist-extract-object-offset /dev/bplin (+ key-pos (* kv-slot sizeof-object)) sizeof-object)]
                                  [voffset (bplist-extract-object-offset /dev/bplin (+ key-pos dict-count (* kv-slot sizeof-object)) sizeof-object)])
                              (define-values (key value)
                                (values (extract-object (bplist-extract-object-position /dev/bplin koffset offset-table-index sizeof-offset trailer-index))
                                        (extract-object (bplist-extract-object-position /dev/bplin voffset offset-table-index sizeof-offset trailer-index))))
                              (when (string? key)
                                (hash-set! dictionary (string->symbol key) value))))
                          dictionary]
                         [(= object-type #b0100)
                          (define-values (byte-count byte-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #false))
                          (bplist-data-ref /dev/bplin byte-pos byte-count)]
                         [(= object-type #b0110)
                          (define-values (char-count char-pos _tag _ln) (bplist-extract-object-size /dev/bplin object-size object-pos #false))
                          (bplist-utf16-ref /dev/bplin char-pos char-count)]
                         [else (void)])]
                  [(= object-size #b1000) #false]
                  [(= object-size #b1001) #true]
                  [else (void)])))))))

(define bplist-write-datum : (-> Any Output-Port Void)
  (lambda [plst /dev/bplout]
    (define magic : Bytes #"bplist00")
    (define object-table-index : Index (bytes-length magic))
    (define-values (unique-objects stcejbo) (bplist-flatten plst))
    (define offset-count : Index (length stcejbo))
    (define sizeof-object : Byte (assert (natural-bytes-length offset-count) byte?))
    (define offset-table : (Vectorof Natural) (make-vector offset-count object-table-index))
    (define pointer-pool : Bytes (make-bytes sizeof-object))
    
    (write-bytes magic /dev/bplout)

    (define offset-table-index : Natural
      (for/fold ([offset-table-index : Natural object-table-index])
                ([object (in-list (reverse stcejbo))]
                 [idx (in-naturals 0)])
        (vector-set! offset-table idx offset-table-index)
        (+ offset-table-index
           (cond [(symbol? object) (bplist-insert-string /dev/bplout (symbol->string object))]
                 [(string? object) (bplist-insert-string /dev/bplout object)]
                 [(exact-integer? object) (bplist-insert-integer /dev/bplout object)]
                 [(flonum? object) (bplist-insert-real /dev/bplout object)]
                 [(vector? object)
                  (define array-size : Index (vector-length object))
                  (define tagsize : Byte (bplist-insert-tag /dev/bplout #b10100000 array-size))
                  (for ([item (in-vector object)])
                    (bplist-insert-pointer /dev/bplout (bplist-unique-ref unique-objects item) sizeof-object pointer-pool))
                  (+ tagsize (* array-size sizeof-object))]
                 [(hash? object)
                  (define dict-size : Index (hash-count object))
                  (define tagsize : Byte (bplist-insert-tag /dev/bplout #b11010000 dict-size))
                  (for ([key (in-hash-keys object)])
                    (bplist-insert-pointer /dev/bplout (bplist-unique-ref unique-objects key) sizeof-object pointer-pool))
                  (for ([value (in-hash-values object)])
                    (bplist-insert-pointer /dev/bplout (bplist-unique-ref unique-objects value) sizeof-object pointer-pool))
                  (+ tagsize (* dict-size 2 sizeof-object))]
                 [(bytes? object) (bplist-insert-data /dev/bplout object)]
                 [(date? object) (bplist-insert-date /dev/bplout object)]
                 [else (bplist-insert-constant /dev/bplout object)]))))
    
    (let ([sizeof-offset (assert (natural-bytes-length (vector-ref offset-table (- offset-count 1))) byte?)])
      (bplist-insert-offset-table /dev/bplout offset-table sizeof-offset)
      (bplist-insert-trailer /dev/bplout sizeof-offset sizeof-object offset-count 0 offset-table-index))
    
    (flush-output /dev/bplout)))

(define bplist-pretty-hexdump : (-> Bytes Output-Port Byte Boolean Void)
  (lambda [/dev/bplin /dev/stdout offset-table-column show-unused-field?]
    (define trailer-index : Fixnum (- (bytes-length /dev/bplin) 32))

    (when (index? trailer-index)
      (define-values (sizeof-offset sizeof-object object-count root-offset offset-table-index) (bplist-extract-trailer /dev/bplin trailer-index))

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
                 (define-values (object-real-size value-idx size-tag size-ln)
                   (bplist-extract-object-size /dev/bplin object-size pos
                                               (and (memq object-type bplist-power-types) #true)))
                 
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
                       [(not (= object-type #b0110))
                        (for ([offset (in-range object-real-size)])
                          (bplist-print-byte /dev/bplin /dev/stdout (+ value-idx offset) #\space values))
                        (print-object-table (+ value-idx object-real-size))]
                       [else ; UTF-16 String
                        (for ([offset (in-range object-real-size)])
                          (bplist-print-integer-octets /dev/bplin /dev/stdout (+ value-idx offset offset) 2 #\space values))
                        (print-object-table (+ value-idx object-real-size object-real-size))])]
                [else (print-object-table (+ pos 1))])))
      
      (bplist-newline /dev/stdout)
      (bplist-newline /dev/stdout)
      
      (let print-offset-table ([pos : Nonnegative-Fixnum offset-table-index]
                               [col : Byte 1])
        (cond [(< pos trailer-index)
               (let ([space (if (= col 0) #\newline #\space)])
                 (bplist-print-integer-octets /dev/bplin /dev/stdout pos sizeof-offset space string-upcase)
                 (print-offset-table (+ pos sizeof-offset) (remainder (+ col 1) offset-table-column)))]
              [(not (= col 1)) (bplist-newline /dev/stdout)]))
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

(define bplist-extract-object-size : (-> Bytes Byte Index Boolean (Values Index Index Byte Byte))
  (lambda [/dev/bplin object-size pos power?]
    (define-values (object-real-size value-idx size-tag size-ln)
      (cond [(= object-size #b1111)
             (let-values ([(size-tag size-ln) (bplist-extract-byte-tag (bytes-ref /dev/bplin (+ pos 1)))])
               (define size-size : Nonnegative-Integer (arithmetic-shift 1 size-ln))
               (define value-idx : Positive-Integer (+ pos 2 size-size))
               (values (network-bytes->natural /dev/bplin (+ pos 2) value-idx) value-idx size-tag size-ln))]
            [(not power?) (values object-size (+ pos 1) 0 0)]
            [else (values (assert (arithmetic-shift 1 object-size) byte?) (+ pos 1) 0 0)]))
    (values object-real-size (assert value-idx index?) size-tag size-ln)))

(define bplist-extract-object-offset : (-> Bytes Integer Byte Index)
  (lambda [/dev/bplin position sizeof-object]
    (network-bytes->natural /dev/bplin position (+ position sizeof-object))))

(define bplist-extract-object-position : (-> Bytes Nonnegative-Integer Index Byte Index Index)
  (lambda [/dev/bplin offset offset-table-index sizeof-offset trailer-index]
    (define pos : Nonnegative-Integer (+ offset-table-index (* offset sizeof-offset)))
    (cond [(>= pos trailer-index) trailer-index]
          [(= sizeof-offset 1) (bytes-ref /dev/bplin pos)]
          [else (network-bytes->natural /dev/bplin pos (+ pos sizeof-offset))])))

(define bplist-extract-offset-table : (-> Bytes Index Byte Index (Listof Index))
  (lambda [/dev/bplin offset-table-index sizeof-offset trailer-index]
    (let extract-offset-table ([pos : Index offset-table-index]
                               [pos++ : Nonnegative-Fixnum (+ offset-table-index sizeof-offset)]
                               [stesffo : (Listof Index) null])
      (cond [(> pos++ trailer-index) (reverse stesffo)]
            [else (extract-offset-table pos++ (+ pos++ sizeof-offset)
                                        (cons (network-bytes->natural /dev/bplin pos pos++)
                                              stesffo))]))))

(define bplist-extract-trailer : (-> Bytes Index (Values Byte Byte Index Index Index))
  (lambda [/dev/bplin trailer-idx]
    (values (bytes-ref /dev/bplin (+ trailer-idx 6))
            (bytes-ref /dev/bplin (+ trailer-idx 7))
            (network-bytes->natural /dev/bplin (+ trailer-idx 8) (+ trailer-idx 16))
            (network-bytes->natural /dev/bplin (+ trailer-idx 16) (+ trailer-idx 24))
            (network-bytes->natural /dev/bplin (+ trailer-idx 24) (+ trailer-idx 32)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-data-ref : (-> Bytes Index Index Bytes)
  (lambda [/dev/bplin pos count]
    (subbytes /dev/bplin pos (+ pos count))))

(define bplist-integer-ref : (-> Bytes Index Index Integer)
  (lambda [/dev/bplin pos count]
    (cond [(< count 8) (network-bytes->natural /dev/bplin pos (+ pos count))]
          [else (network-bytes->integer /dev/bplin pos (+ pos count))])))

(define bplist-real-ref : (-> Bytes Index Index Flonum)
  (lambda [/dev/bplin pos count]
    (case count
      [(4 8) (floating-point-bytes->real /dev/bplin #true pos (+ pos count))]
      [else +nan.0])))

(define bplist-string-ref : (-> Bytes Index Index String)
  (lambda [/dev/bplin pos count]
    (bytes->string/utf-8 /dev/bplin #\uFFFD pos (+ pos count))))

(define bplist-utf16-ref : (-> Bytes Index Index String)
  (lambda [/dev/bplin pos count]
    (define-values (/dev/u16in /dev/u16out) (make-pipe))
    (define /dev/utfin (reencode-input-port /dev/u16in "UTF-16BE" #false #true))

    (write-bytes /dev/bplin /dev/u16out pos (+ pos count count))
    (close-output-port /dev/u16out)
    (bytes->string/utf-8 (port->bytes /dev/utfin))))

(define bplist-date-ref : (-> Bytes Nonnegative-Fixnum date)
  (lambda [/dev/bplin pos]
    (define utc-s : Flonum (floating-point-bytes->real /dev/bplin #true pos (+ pos 8)))
    (seconds->date utc-s #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-flatten : (->* (Any) (BPList-Unique-Objects (Listof Any)) (Values BPList-Unique-Objects (Listof Any)))
  (lambda [object [unique-objects pblist-empty-unqiues] [stcejbo null]]
    (define key : Any (bplist-unique-key object))
    (cond [(hash-has-key? unique-objects key) (values unique-objects stcejbo)]
          [(hash? object)
           (for/fold ([uniques : BPList-Unique-Objects (bplist-unique-set unique-objects key stcejbo)]
                      [objects : (Listof Any) (cons object stcejbo)])
                     ([(key value) (in-hash object)])
             (define-values (key-flattened key-counted) (bplist-flatten key uniques objects))
             (bplist-flatten value key-flattened key-counted))]
          [(vector? object)
           (for/fold ([uniques : BPList-Unique-Objects (bplist-unique-set unique-objects key stcejbo)]
                      [objects : (Listof Any) (cons object stcejbo)])
                     ([item (in-vector object)])
             (bplist-flatten item uniques objects))]
          [else (values (hash-set unique-objects key (length stcejbo)) (cons object stcejbo))])))

(define bplist-unique-key : (-> Any Any)
  (lambda [object]
    (cond [(vector? object) (box (eq-hash-code object))]
          [(hash? object) (box (eq-hash-code object))]
          [else object])))

(define bplist-unique-set : (-> BPList-Unique-Objects Any (Listof Any) BPList-Unique-Objects)
  (lambda [uniques key objects]
    (cond [(hash-has-key? uniques key) uniques]
          [else (hash-set uniques key (length objects))])))

(define bplist-unique-ref : (-> BPList-Unique-Objects Any Natural)
  (lambda [uniques object]
    (hash-ref uniques (bplist-unique-key object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-insert-tag : (-> Output-Port Byte Index Byte)
  (lambda [/dev/bplout tag0000 size]    
    (cond [(< size #b1111)
           (write-byte (bitwise-xor tag0000 size) /dev/bplout)
           1]
          [(<= size #xFF)
           (write-byte (bitwise-xor tag0000 #b1111) /dev/bplout)
           (write-byte #b00010000 /dev/bplout)
           (write-byte size /dev/bplout)
           3]
          [(<= size #xFFFF)
           (write-byte (bitwise-xor tag0000 #b1111) /dev/bplout)
           (write-byte #b00010001 /dev/bplout)
           (write-bytes (natural->network-bytes size 2) /dev/bplout 0 2)
           4]
          [(<= size #xFFFFFFFF)
           (write-byte (bitwise-xor tag0000 #b1111) /dev/bplout)
           (write-byte #b00010010 /dev/bplout)
           (write-bytes (natural->network-bytes size 4) /dev/bplout 0 4)
           6]
          [else #| uint64 |#
           (write-byte (bitwise-xor tag0000 #b1111) /dev/bplout)
           (write-byte #b00010011 /dev/bplout)
           (write-bytes (natural->network-bytes size 8) /dev/bplout 0 4)
           10])))

(define bplist-insert-data : (-> Output-Port Bytes Natural)
  (lambda [/dev/bplout content]
    (+ (bplist-insert-tag /dev/bplout #b01000000 (bytes-length content))
       (write-bytes content /dev/bplout))))

(define bplist-insert-string : (-> Output-Port String Natural)
  (lambda [/dev/bplout content]
    (define /dev/bytout : Output-Port (open-output-bytes))

    (define ascii? : Boolean
      (for/fold ([ascii? : Boolean #true])
                ([ch (in-string content)])
        (write-char ch /dev/bytout)
        (and ascii? (char<=? ch #\rubout))))
    
    (let ([bs (get-output-bytes /dev/bytout #true)])
      (+ (bplist-insert-tag /dev/bplout (if ascii? #b01010000 #b01110000) (bytes-length bs))
         (write-bytes bs /dev/bplout)))))

(define bplist-insert-integer : (-> Output-Port Integer Natural)
  (lambda [/dev/bplout n]
    (cond [(< n #x00)
           (write-byte #b00010011 /dev/bplout)
           (write-bytes (integer->network-bytes n 8) /dev/bplout)
           9]
          [(<= n #xFF)
           (write-byte #b00010000 /dev/bplout)
           (write-byte n /dev/bplout)
           2]
          [(<= n #xFFFF)
           (write-byte #b00010001 /dev/bplout)
           (write-bytes (natural->network-bytes n 2) /dev/bplout)
           3]
          [(<= n #xFFFFFFFF)
           (write-byte #b00010010 /dev/bplout)
           (write-bytes (natural->network-bytes n 4) /dev/bplout)
           5]
          [else #| int64 |#
           (write-byte #b00010011 /dev/bplout)
           (write-bytes (integer->network-bytes n 8) /dev/bplout)
           9])))

(define bplist-insert-real : (-> Output-Port Flonum Natural)
  (lambda [/dev/bplout r]
    (write-byte #b00100011 /dev/bplout)
    (write-bytes (real->floating-point-bytes r 8 #true) /dev/bplout)
    9))

(define bplist-insert-date : (-> Output-Port date Natural)
  (lambda [/dev/bplout d]
    (write-byte #b00110011 /dev/bplout)
    (write-bytes (real->floating-point-bytes (date*->seconds d #false) 8 #true) /dev/bplout)
    9))

(define bplist-insert-constant : (-> Output-Port Any Byte)
  (lambda [/dev/bplout datum]
    (cond [(eq? datum #true) (write-byte #b00001001 /dev/bplout)]
          [(eq? datum #false) (write-byte #b00001000 /dev/bplout)]
          [(void? datum) (write-byte #b00000000 /dev/bplout)]
          [else (write-byte #b00001111 /dev/bplout)])
    1))

(define bplist-insert-pointer : (-> Output-Port Natural Index Bytes Void)
  (lambda [/dev/bplout offset sizeof-object pool]
    (cond [(= sizeof-object 1) (write-byte offset /dev/bplout)]
          [else (void (write-bytes (natural->network-bytes offset sizeof-object pool 0)
                                   /dev/bplout 0 sizeof-object))])))

(define bplist-insert-offset-table : (-> Output-Port (Vectorof Natural) Index Void)
  (lambda [/dev/bplout offsets sizeof-offset]
    (define pool : Bytes (make-bytes sizeof-offset))

    (for ([offset (in-vector offsets)])
      (natural->network-bytes offset sizeof-offset pool)
      (write-bytes pool /dev/bplout 0 sizeof-offset))))

(define bplist-insert-trailer : (-> Output-Port Byte Byte Natural Natural Natural Void)
  (lambda [/dev/bplout sizeof-offset sizeof-object object-count root-offset offset-table-index]
    (define pool : Bytes (make-bytes 24 0))

    (write-bytes pool /dev/bplout 0 6)
    (write-byte sizeof-offset /dev/bplout)
    (write-byte sizeof-object /dev/bplout)

    (natural->network-bytes object-count 8 pool 0)
    (natural->network-bytes root-offset 8 pool 8)
    (natural->network-bytes offset-table-index 8 pool 16)
    (write-bytes pool /dev/bplout 0 24)
    (void)))
