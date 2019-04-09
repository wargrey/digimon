#lang typed/racket/base

(provide (all-defined-out) plural)
(provide (all-from-out typed/racket/date))
(provide (all-from-out racket/format))
(provide (all-from-out racket/pretty))

(require "digitama/sugar.rkt")
(require "digitama/plural.rkt")

(require racket/format)
(require racket/flonum)
(require racket/pretty)
(require racket/string)
(require racket/math)

(require typed/racket/date)

(define ~n_w : (-> Integer String String)
  (lambda [count word]
    (format "~a ~a" count (plural count word))))

(define ~w=n : (-> Integer String String)
  (lambda [count word]
    (format "~a=~a" (plural count word) count)))

(define ~% : (-> Flonum [#:precision (U Integer (List '= Integer))] String)
  (lambda [% #:precision [prcs '(= 2)]]
    (string-append (~r (fl* 100.0 %) #:precision prcs) "%")))

(define ~uptime : (-> Integer String)
  (let ([~t : (-> Real String) (λ [n] (if (< n 10) (string-append "0" (number->string n)) (number->string n)))])
    (lambda [s]
      (let*-values ([(d s) (quotient/remainder s 86400)]
                    [(h s) (quotient/remainder s 3600)]
                    [(m s) (quotient/remainder s 60)])
        (cond [(zero? d) (format "~a:~a:~a" (~t h) (~t m) (~t s))]
              [else (format "~a+~a:~a:~a" d (~t h) (~t m) (~t s))])))))

(define ~gctime : (-> Integer String)
  (lambda [ms]
    (let*-values ([(s ms) (quotient/remainder ms 1000)]
                  [(m s) (quotient/remainder s 60)])
      (define padding : String (cond [(< ms 10) "00"] [(< ms 100) "0"] [else ""]))
      (cond [(zero? m) (format "~a.~a~a" s padding ms)]
            [else (format "~a:~a.~a~a" m s padding ms)]))))

(define-type/enum units : Unit 'KB 'MB 'GB 'TB)
(define ~size : (->* (Real) ((U 'Bytes Unit) #:precision (U Integer (List '= Integer))) String)
  (lambda [size [unit 'Bytes] #:precision [prcs '(= 3)]]
    (if (eq? unit 'Bytes)
        (cond [(< -1024.0 size 1024.0) (~n_w (exact-round size) "Byte")]
              [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
        (let try-next-unit : String ([s : Flonum (real->double-flonum size)] [us : (Option Unit*) (memq unit units)])
          (cond [(not us) "Typed Racket is buggy if you see this message"]
                [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define byte->hex-string : (-> Byte String)
  (lambda [b]
    (~r b #:base 16 #:min-width 2 #:pad-string "0")))

(define byte->bin-string : (-> Byte String)
  (lambda [b]
    (~r b #:base 2 #:min-width 8 #:pad-string "0")))

(define bytes->hex-string : (-> Bytes [#:separator String] String)
  (lambda [bstr #:separator [sep ""]]
    (string-join (for/list : (Listof String) ([b (in-bytes bstr)]) (byte->hex-string b))
                 sep)))

(define bytes->bin-string : (-> Bytes [#:separator String] String)
  (lambda [bstr #:separator [sep ""]]
    (string-join (for/list : (Listof String) ([b (in-bytes bstr)]) (byte->bin-string b))
                 sep)))

(define symb0x->number : (-> Symbol (Option Integer))
  (lambda [hex]
    (define maybe-integer : (Option Number) (string->number (substring (symbol->string hex) 2) 16))
    (and (exact-integer? maybe-integer) maybe-integer)))

(define number->symb0x : (-> Integer Symbol)
  (lambda [mphex]
    (string->symbol (string-append "0x" (number->string mphex 16)))))
