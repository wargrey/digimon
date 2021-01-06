#lang typed/racket/base

(provide (all-defined-out) plural)
(provide (all-from-out racket/format))
(provide (all-from-out racket/pretty))

(require "digitama/plural.rkt")

(require "enumeration.rkt")

(require racket/flonum)
(require racket/string)
(require racket/symbol)
(require racket/math)

(require racket/format)
(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~n_w : (-> Integer String String)
  (lambda [count word]
    (format "~a ~a" count (plural count word))))

(define ~w=n : (-> Integer String String)
  (lambda [count word]
    (format "~a=~a" (plural count word) count)))

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
          [(bytes? val) (bytes->hex-string val #:separator " ")]
          [(boolean? val) (~hexstring (if val 1 0))]
          [else (~hexstring (string->bytes/utf-8 (~a val)))])))

(define ~binstring : (-> Any String)
  (lambda [val]
    (cond [(integer? val) (~r val #:base 2)]
          [(bytes? val) (bytes->bin-string val #:separator " ")]
          [(boolean? val) (~binstring (if val 1 0))]
          [else (~binstring (string->bytes/utf-8 (~a val)))])))

(define ~space : (-> Natural String)
  (let ([space : (HashTable Natural String) (make-hasheq)])
    (lambda [n]
      (hash-ref! space n
                 (λ [] (make-string n #\space))))))

(define ~string : (-> String (Listof Any) String)
  (lambda [msgfmt argl]
    (if (null? argl) msgfmt (apply format msgfmt argl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define generate-immutable-string : (-> (U String Symbol) String)
  (lambda [base]
    (symbol->immutable-string (gensym base))))

(define byte->hex-string : (-> Byte String)
  (lambda [b]
    (define hex (number->string b 16))
    
    (if (> b #xF) hex (string-append "0" hex))))

(define byte->bin-string : (-> Byte String)
  (lambda [b]
    (~r b #:base 2 #:min-width 8 #:pad-string "0")))

(define bytes->hex-string : (->* (Bytes) (Natural (Option Natural) Natural #:separator String) String)
  (lambda [bstr [start 0] [stop #false] [step 1] #:separator [sep ""]]
    (string-join (for/list : (Listof String) ([b (in-bytes bstr start stop step)])
                   (byte->hex-string b))
                 sep)))

(define bytes->bin-string : (->* (Bytes) (Natural (Option Natural) Natural #:separator String) String)
  (lambda [bstr [start 0] [stop #false] [step 1] #:separator [sep ""]]
    (string-join (for/list : (Listof String) ([b (in-bytes bstr start stop step)])
                   (byte->bin-string b))
                 sep)))

(define symb0x->number : (-> Symbol (Option Integer))
  (lambda [hex]
    (define maybe-integer : (Option Number) (string->number (substring (symbol->immutable-string hex) 2) 16))
    (and (exact-integer? maybe-integer) maybe-integer)))

(define number->symb0x : (-> Integer Symbol)
  (lambda [mphex]
    (string->symbol (string-append "0x" (number->string mphex 16)))))

(define symb0b->number : (-> Symbol (Option Integer))
  (lambda [bin]
    (define maybe-integer : (Option Number) (string->number (substring (symbol->immutable-string bin) 2) 2))
    (and (exact-integer? maybe-integer) maybe-integer)))

(define number->symb0b : (-> Integer Symbol)
  (lambda [mpbin]
    (string->symbol (string-append "0b" (number->string mpbin 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-column-widths : (-> (Pairof (Listof String) (Listof (Listof String))) (Listof Natural))
  (lambda [entries]
    (for/fold ([widths : (Listof Natural) (map string-length (car entries))])
              ([columns : (Listof String) (in-list (cdr entries))])
      (for/list ([col (in-list columns)]
                 [width (in-list widths)])
        (max width (string-length col))))))
