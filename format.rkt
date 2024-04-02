#lang typed/racket/base

(provide (all-defined-out) plural)
(provide (all-from-out racket/format))
(provide (all-from-out racket/pretty))
(provide (all-from-out "digitama/minimal/string.rkt"))
(provide (all-from-out "digitama/minimal/format.rkt"))
(provide (rename-out [symb0x->octets symb0x->bytes]))

(require "digitama/plural.rkt")
(require "digitama/minimal/format.rkt")
(require "digitama/minimal/string.rkt")

(require "character.rkt")

(require racket/string)
(require racket/symbol)

(require racket/format)
(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
