#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out typed/racket/date))

(require "digitama/sugar.rkt")
(require "digitama/plural.rkt")

(require racket/flonum)
(require racket/fixnum)

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
          (cond [(false? us) "Typed Racket is buggy if you see this message"]
                [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))
