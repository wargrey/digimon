#lang typed/racket/base

(provide (all-defined-out))

(require "../plural.rkt")
(require "string.rkt")

(require "../../enumeration.rkt")

(require racket/flonum)
(require racket/symbol)
(require racket/math)

(require racket/format)

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

(define ~integer : (-> Integer String)
  (lambda [i]
    (cond [(< i 0) (string-append "-" (~integer (- i)))]
          [(< i 1000) (number->string i)]
          [else (let integer->string ([head : Natural (quotient i 1000)]
                                      [tail : String (string-append "," (~r (remainder i 1000) #:min-width 3 #:pad-string "0"))])
                  (cond [(< head 1000) (string-append (number->string head) tail)]
                        [else (let-values ([(q r) (quotient/remainder head 1000)])
                                (integer->string q (string-append "," (~r r #:min-width 3) tail)))]))])))

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

(define ~MB/s : (-> Natural Flonum Flonum)
  (lambda [traffic span-ms]
    (/ (real->double-flonum traffic)
       (* span-ms 1024.0 1.024))))

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

