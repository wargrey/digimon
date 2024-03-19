#lang typed/racket/base

(provide (all-defined-out) plural)
(provide (all-from-out racket/format))
(provide (all-from-out racket/pretty))
(provide (rename-out [symb0x->octets symb0x->bytes]))

(require "digitama/plural.rkt")

(require "enumeration.rkt")
(require "character.rkt")

(require racket/flonum)
(require racket/string)
(require racket/symbol)
(require racket/math)
(require racket/port)

(require racket/format)
(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~n_w : (->* (Integer String) (Boolean) String)
  (lambda [count word [format-count? #true]]
    (format "~a ~a"
      (if format-count? (~integer count) count)
      (plural count word))))

(define ~w=n : (->* (Integer String) (Boolean) String)
  (lambda [count word [format-count? #true]]
    (format "~a=~a"
      (plural count word)
      (if format-count? (~integer count) count))))

(define ~% : (-> Real [#:precision (U Integer (List '= Integer))] String)
  (lambda [% #:precision [prcs '(= 2)]]
    (string-append (~r (fl* 100.0 (real->double-flonum %)) #:precision prcs) "%")))

(define ~0time : (-> Real String)
  (lambda [n]
    (cond [(< n 10) (string-append "0" (number->string n))]
          [else (number->string n)])))

(define ~uptime : (-> Integer String)
  (lambda [s]
    (let*-values ([(d s) (quotient/remainder s 86400)]
                  [(h s) (quotient/remainder s 3600)]
                  [(m s) (quotient/remainder s 60)])
      (cond [(zero? d) (format "~a:~a:~a" (~0time h) (~0time m) (~0time s))]
            [else (format "~a+~a:~a:~a" d (~0time h) (~0time m) (~0time s))]))))

(define ~gctime : (-> Integer String)
  (lambda [ms]
    (let*-values ([(s ms) (quotient/remainder ms 1000)]
                  [(m s) (quotient/remainder s 60)])
      (define padding : String (cond [(< ms 10) "00"] [(< ms 100) "0"] [else ""]))
      (cond [(zero? m) (format "~a.~a~a" s padding ms)]
            [else (format "~a:~a.~a~a" m s padding ms)]))))

(define-enumeration unit : Unit [KB MB GB TB PB EB ZB YB])
(define ~size : (->* (Real) ((U 'Bytes Unit) #:precision (U Integer (List '= Integer)) #:bytes->string (-> Integer String String)) String)
  (lambda [size [unit 'Bytes] #:precision [prcs '(= 3)] #:bytes->string [bytes->string ~n_w]]
    (if (eq? unit 'Bytes)
        (cond [(< -1024.0 size 1024.0) (bytes->string (exact-round size) "Byte")]
              [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
        (let try-next-unit : String ([s : Flonum (real->double-flonum size)] [us : (Option (Listof Unit)) (memq unit units)])
          (cond [(not us) "Typed Racket is buggy if you see this message"]
                [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->immutable-string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))

(define ~MB/s : (-> Natural Flonum Flonum)
  (lambda [traffic span-ms]
    (/ (real->double-flonum traffic)
       (* span-ms 1024.0 1.024))))

(define ~hexstring : (-> Any String)
  (lambda [val]
    (cond [(integer? val) (~r val #:base 16)]
          [(bytes? val) (bytes->hexstring val #:separator " ")]
          [(boolean? val) (~hexstring (if val 1 0))]
          [else (~hexstring (string->bytes/utf-8 (~a val)))])))

(define ~binstring : (->* (Any) (Integer) String)
  (lambda [val [width 0]]
    (cond [(integer? val) (if (<= width 0) (~r val #:base 2) (~r val #:base 2 #:min-width width #:pad-string "0"))]
          [(bytes? val) (bytes->binstring val #:separator " ")]
          [(boolean? val) (~binstring (if val 1 0))]
          [else (~binstring (string->bytes/utf-8 (~a val)))])))

(define ~space : (-> Integer String)
  (let ([space : (HashTable Integer String) (make-hasheq)])
    (lambda [n0]
      (define n : Index (if (index? n0) n0 0))
      
      (hash-ref! space n
                 (λ [] (make-string n #\space))))))

(define ~char : (case-> [(U Char Integer) String -> String]
                        [(U Char Integer) Char -> Char]
                        [(U Char Integer) -> String])
  (case-lambda
    [(ch) (~char ch ".")]
    [(c fallback)
     (define ch : Char (if (char? c) c (integer->char c)))
     
     (if (string? fallback)
         (cond [(char-graphic? ch) (string ch)]
               [(char-whitespace? ch) " "]
               [else fallback])
         (cond [(char-graphic? ch) ch]
               [(char-whitespace? ch) #\space]
               [else fallback]))]))

(define ~integer : (-> Integer String)
  (lambda [i]
    (cond [(< i 0) (string-append "-" (~integer (- i)))]
          [(< i 1000) (number->string i)]
          [else (let integer->string ([head : Natural (quotient i 1000)]
                                      [tail : String (string-append "," (~r (remainder i 1000) #:min-width 3 #:pad-string "0"))])
                  (cond [(< head 1000) (string-append (number->string head) tail)]
                        [else (let-values ([(q r) (quotient/remainder head 1000)])
                                (integer->string q (string-append "," (~r r #:min-width 3) tail)))]))])))

(define ~string : (-> String (Listof Any) String)
  (lambda [msgfmt argl]
    (if (null? argl) msgfmt (apply format msgfmt argl))))

(define ~string-lines : (-> String (Pairof String (Listof String)))
  (let ([empty-lines (list "")])
    (lambda [s]
      (if (regexp-match? #px"[\r\n]+" s)
          (let ([lines (call-with-input-string s port->lines)])
            (cond [(null? lines) empty-lines]
                  [else lines]))
          (list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define generate-immutable-string : (-> (U String Symbol) String)
  (lambda [base]
    (symbol->immutable-string (gensym base))))

(define byte->hexstring : (->* (Byte) (Boolean) String)
  (lambda [b [upcase? #false]]
    (define hex : String
      (cond [(not upcase?) (number->string b 16)]
            [else (string-upcase (number->string b 16))]))
    
    (if (> b #xF) hex (string-append "0" hex))))

(define hexdigits->byte : (-> Char Char Byte)
  (lambda [hd1 hd2]
    (assert (+ (* (char->hexadecimal hd1) 16)
               (char->hexadecimal hd2))  byte?)))

(define byte->binstring : (->* (Byte) (Integer) String)
  (lambda [b [width 8]]
    (~r b #:base 2 #:min-width (if (<= width 0) 8 width) #:pad-string "0")))

(define bytes->hexstring : (->* (Bytes)
                                (Natural (Option Natural) Natural
                                         #:separator String #:upcase? Boolean
                                         #:before-first String #:after-last String)
                                String)
  (lambda [bstr [start 0] [stop #false] [step 1]
                #:separator [sep ""] #:upcase? [upcase? #false]
                #:before-first [before ""] #:after-last [after ""]]
    (string-join #:before-first before #:after-last after
                 (for/list : (Listof String) ([b (in-bytes bstr start stop step)])
                   (byte->hexstring b upcase?))
                 sep)))

(define bytes->binstring : (->* (Bytes)
                                (Natural (Option Natural) Natural
                                         #:separator String #:before-first String #:after-last String)
                                String)
  (lambda [bstr [start 0] [stop #false] [step 1]
                #:separator [sep ""] #:before-first [before ""] #:after-last [after ""]]
    (string-join #:before-first before #:after-last after
                 (for/list : (Listof String) ([b (in-bytes bstr start stop step)])
                   (byte->binstring b))
                 sep)))


(define hexstring->bytes : (->* (String) (Natural (Option Natural) #:error-byte (Option Byte)) Bytes)
  (lambda [str [start 0] [stop #false] #:error-byte [err-byte #false]]
    (define ssize : Index (string-length str))
    (define maxidx : Index (if stop (assert (min ssize stop) index?) ssize))

    (with-asserts ([start index?])
      (let string->bytes ([pos+0 : Nonnegative-Fixnum start]
                          [pos+1 : Nonnegative-Fixnum (+ start 1)]
                          [setyb : (Listof Byte) null])
        (if (< pos+1 maxidx)
            (let ([hd1 (string-ref str pos+0)]
                  [hd2 (string-ref str pos+1)])
              (cond [(and (char-hexdigit? hd1) (char-hexdigit? hd2))
                     (string->bytes (+ pos+1 1) (+ pos+1 2)
                                    (cons (hexdigits->byte hd1 hd2) setyb))]
                    [(char-blank? hd1) (string->bytes pos+1 (+ pos+1 1) setyb)]
                    [(not err-byte) (error 'hexstring->bytes "invalid hexadecimal: ~a~a@~a" hd1 hd2 pos+0)]
                    [else (string->bytes (+ pos+1 1) (+ pos+1 2) (cons err-byte setyb))]))
            (apply bytes (reverse setyb)))))))

(define hexstring->bytes! : (->* (String Bytes) (Natural #:error-byte (Option Byte)) Index)
  (lambda [str bs [start 0] #:error-byte [err-byte #false]]
    (define size : Index (string-length str))
    (define n : Index (bytes-length bs))

    (let string->bytes ([pos+0 : Nonnegative-Fixnum 0]
                        [pos+1 : Nonnegative-Fixnum 1]
                        [idx : Natural start])
      (if (and (< idx n) (< pos+1 size))
          (let ([hd1 (string-ref str pos+0)]
                [hd2 (string-ref str pos+1)])
            (cond [(and (char-hexdigit? hd1) (char-hexdigit? hd2))
                   (bytes-set! bs idx (hexdigits->byte hd1 hd2))
                   (string->bytes (+ pos+1 1) (+ pos+1 2) (+ idx 1))]
                  [(char-blank? hd1) (string->bytes pos+1 (+ pos+1 1) idx)]
                  [(byte? err-byte)
                   (bytes-set! bs idx err-byte)
                   (string->bytes (+ pos+1 1) (+ pos+1 2) idx)]
                  [else (error 'hexstring->bytes! "invalid hexadecimal: ~a~a@~a" hd1 hd2 pos+0)]))
          (assert idx index?)))))

(define symb0x->octets : (-> Symbol (Option Bytes))
  (lambda [sym]
    (define str : String (substring (symbol->immutable-string sym) 2))
    (define estr : String (if (odd? (string-length str)) (string-append "0" str) str))
    
    (let string->octets ([bs : (Listof Byte) null]
                         [size : Nonnegative-Fixnum (string-length estr)])
      (define size-2 : Fixnum (- size 2))
      (if (>= size-2 0)
          (let* ([10s (char->hexadecimal (string-ref estr size-2))]
                 [1s (char->hexadecimal (string-ref estr (+ size-2 1)))]
                 [b (+ (* 10s 16) 1s)])
            (and (byte? b)
                 (string->octets (cons b bs) size-2)))
          (apply bytes bs)))))

(define symb0x->number : (-> Symbol (Option Integer))
  (lambda [hex]
    (define maybe-hex (regexp-match #px"^([+-]?)0x([a-fA-F0-9]+)" (symbol->immutable-string hex)))

    (and maybe-hex (pair? (cdr maybe-hex)) (pair? (cddr maybe-hex))
         (let* ([sgn (cadr maybe-hex)]
                [maybe-integer (string->number (assert (caddr maybe-hex)) 16)])
           (and (exact-integer? maybe-integer)
                (if (equal? sgn "-")
                    (- maybe-integer)
                    maybe-integer))))))

(define number->symb0x : (-> Integer Symbol)
  (lambda [mphex]
    (if (>= mphex 0)
        (string->symbol (string-append "0x" (number->string mphex 16)))
        (string->symbol (string-append "-0x" (number->string (- mphex) 16))))))

(define symb0b->number : (-> Symbol (Option Integer))
  (lambda [bin]
    (define maybe-bin (regexp-match #px"^([+-]?)0b([01]+)" (symbol->immutable-string bin)))

    (and maybe-bin (pair? (cdr maybe-bin)) (pair? (cddr maybe-bin))
         (let* ([sgn (cadr maybe-bin)]
                [maybe-integer (string->number (assert (caddr maybe-bin)) 2)])
           (and (exact-integer? maybe-integer)
                (if (equal? sgn "-")
                    (- maybe-integer)
                    maybe-integer))))))

(define number->symb0b : (-> Integer Symbol)
  (lambda [mpbin]
    (if (>= mpbin 0)
        (string->symbol (string-append "0b" (number->string mpbin 2)))
        (string->symbol (string-append "-0b" (number->string (- mpbin) 2))))))

(define natural->string : (->* (Natural) ((U Integer (List 'up Integer)) Positive-Integer) String)
  (lambda [n [radix (list 'up 16)] [width 1]]
    (string-append (case radix [(2) "#b"] [(8) "#o"] [(16) "#x"] [else ""])
                   (~r n #:base radix #:min-width width #:pad-string "0"))))

(define string->quoted-symbol : (-> String Symbol)
  (lambda [str]
    (string->symbol
     (string-append "\""
                    (format "~a" str)
                    "\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-column-widths : (-> (Pairof (Listof String) (Listof (Listof String))) (Listof Index))
  (lambda [entries]
    (for/fold ([widths : (Listof Index) (map string-length (car entries))])
              ([columns : (Listof String) (in-list (cdr entries))])
      (for/list ([col (in-list columns)]
                 [width (in-list widths)])
        (let ([self-width (string-length col)])
          (cond [(>= width self-width) width]
                [else self-width]))))))
