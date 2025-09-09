#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)
(require typed/racket/date)
(require typed/racket/unsafe)

(require "minimal/format.rkt")
(require "../tongue.rkt")


(unsafe-require/typed 
 racket/base
 [#:struct (date* date) ([nanosecond : Natural] [time-zone-name : String])
  #:extra-constructor-name make-date*])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~strftime : (->* (date String (Option Symbol)) (Output-Port Boolean) Void)
  (lambda [self tfmt nation [/dev/tmout (current-output-port)] [silent? #true]]
    (define size : Index (string-length tfmt))
    
    (let subftime : Void ([fmt : String tfmt]
                          [idx : Nonnegative-Fixnum 0]
                          [%? : Boolean #false])
      (if (< idx size)
          (let ([ch (string-ref fmt idx)])
            (cond [(and %?)
                   (case/eq ch
                     [(#\A) (write-string (~weekday/full (date-week-day self) nation) /dev/tmout)]
                     [(#\a) (write-string (~weekday/abbr (date-week-day self) nation) /dev/tmout)]
                     [(#\B) (write-string (~month/full (date-month self) nation) /dev/tmout)]
                     [(#\b) (write-string (~month/abbr (date-month self) nation) /dev/tmout)]
                     [(#\C) (write-string (~0time (quotient (date-year self) 100)) /dev/tmout)]
                     [(#\c) (~strftime self (~localize '%c nation) nation /dev/tmout)]
                     [(#\D) (~strftime self "%m/%d/%y" nation /dev/tmout #true)]
                     [(#\d) (write-string (~0time (date-day self)) /dev/tmout)]
                     [(#\e) (write-string (~_time (date-day self)) /dev/tmout)]
                     [(#\F) (~strftime self "%Y-%m-%d" nation /dev/tmout #true)]
                     [(#\G) (write (date-year self) /dev/tmout)] ; TODO, based on year-week, monday first
                     [(#\g) (write (remainder (date-year self) 100) /dev/tmout)] ; TODO: same as above
                     [(#\H) (write-string (~0time (date-hour self)) /dev/tmout)] ; [0, 23]
                     [(#\h) (write-string (~month/abbr (date-month self) nation) /dev/tmout)] ; same as %b
                     [(#\I) (write-string (~0time (~hour12 (date-hour self))) /dev/tmout)]
                     [(#\j) (write-string (~0year (add1 (date-year-day self))) /dev/tmout)]
                     [(#\k) (write-string (~_time (date-hour self)) /dev/tmout)]
                     [(#\l) (write-string (~_time (~hour12 (date-hour self))) /dev/tmout)]
                     [(#\M) (write-string (~0time (date-minute self)) /dev/tmout)]
                     [(#\m) (write-string (~0time (date-month self)) /dev/tmout)]
                     [(#\n) (newline /dev/tmout)]
                     [(#\p) (write-string (~am/pm (date-hour self) nation) /dev/tmout)]
                     [(#\R) (~strftime self "%H:%M" nation /dev/tmout #true)]
                     [(#\r) (~strftime self "%I:%M:%S %p" nation /dev/tmout #true)]
                     [(#\S) (write-string (~0time (date-second self)) /dev/tmout)]
                     [(#\s) (write (date->seconds self #false) /dev/tmout)]
                     [(#\T) (~strftime self "%H:%M:%S" nation /dev/tmout #true)]
                     [(#\t) (write-char #\t /dev/tmout)]
                     [(#\U) (write (+ (date-week-day self) 1) /dev/tmout)] ; Sunday first, [00, 53]
                     [(#\u) (write (~weekday/sunday (date-week-day self)) /dev/tmout)] ; Monday first, [1, 7]
                     [(#\V) (write (+ (date-week-day self) 1) /dev/tmout)] ; Monday first, [01, 53]
                     [(#\v) (~strftime self "%e-%b-%Y" nation /dev/tmout #true)]
                     [(#\W) (write (+ (date-week-day self) 1) /dev/tmout)] ; Monday first, [00, 53]
                     [(#\w) (write (date-week-day self) /dev/tmout)] ; Sunday first, [0, 6]
                     [(#\X) (~strftime self (~localize '%X nation) nation /dev/tmout)]
                     [(#\x) (~strftime self (~localize '%x nation) nation /dev/tmout)]
                     [(#\Y) (write (date-year self) /dev/tmout)]
                     [(#\y) (write (remainder (date-year self) 100) /dev/tmout)]
                     [(#\Z) (write-string (date*-time-zone-name (assert self date*?)) /dev/tmout)]
                     [(#\z) (write-string (~tz-offset self) /dev/tmout)]
                     [(#\+) (~strftime self (~localize '%+ nation) nation /dev/tmout)]
                     [(#\%) (write-char #\% /dev/tmout)]
                     [else (and (not silent?)
                                (raise-syntax-error 'strftime
                                                    (format "unrecognized conversion specification@~a" idx)
                                                    '|format string|
                                                    (datum->syntax #false fmt
                                                                   (vector #false 1 idx (+ idx 1) 1))))])
                   (subftime fmt (+ idx 1) #false)]
                  [(not (eq? ch #\%))
                   (write-char ch /dev/tmout)
                   (subftime fmt (+ idx 1) #false)]
                  [else (subftime fmt (+ idx 1) #true)]))
          (when (and %? (not silent?))
            (raise-syntax-error 'strftime "terminated with %" 'format))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~tz-offset : (-> date String)
  (lambda [d]
    (define delta : Integer (date-time-zone-offset d))
    (define hours : Integer (quotient delta 3600))
    (define minutes : Integer (modulo (quotient delta 60) 60))

    (string-append (if (negative? delta) "-" "+")
                   (~0time (abs hours))
                   (~0time minutes))))

(define ~localize : (-> Symbol (Option Symbol) String)
  (lambda [% nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           %)))

(define ~am/pm : (-> Natural (Option Symbol) String)
  (lambda [h nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           (if (< h 12) 'am 'pm))))

(define ~month/full : (-> Natural (Option Symbol) String)
  (lambda [m nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           (case/eq m
             [(1)  'January] [(2)  'February]  [(3) 'March]
             [(4)  'April]   [(5)  'May]       [(6) 'June]
             [(7)  'July]    [(8)  'August]    [(9) 'September]
             [(10) 'October] [(11) 'November]
             [else 'December]))))

(define ~month/abbr : (-> Natural (Option Symbol) String)
  (lambda [m nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           (case/eq m
             [(1)  'jan] [(2)  'feb] [(3) 'mar]
             [(4)  'apr] [(5)  'may] [(6) 'jun]
             [(7)  'jul] [(8)  'aug] [(9) 'sep]
             [(10) 'oct] [(11) 'nov]
             [else 'dec]))))

(define ~weekday/full : (-> Natural (Option Symbol) String)
  (lambda [w nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           (case/eq w
             [(0)  'Sunday]    [(1) 'Monday]   [(2) 'Tuesday]
             [(3)  'Wednesday] [(4) 'Thursday] [(5) 'Friday]
             [else 'Saturday]))))

(define ~weekday/abbr : (-> Natural (Option Symbol) String)
  (lambda [w nation]
    (speak #:in (or nation (current-tongue))
           #:dialect 'strftime
           (case/eq w
             [(0)  'sun] [(1) 'mon] [(2) 'tue]
             [(3)  'wed] [(4) 'thu] [(5) 'fri]
             [else 'sat]))))

(define ~weekday/sunday : (-> Natural Natural)
  (lambda [w]
    (if (= w 0) 7 w)))

(define ~hour12 : (-> Natural Integer)
  (lambda [h]
    (cond [(zero? h) 12]
          [(> h 12) (- h 12)]
          [else h])))

(define ~0year : (-> Integer String)
  (lambda [t]
    (cond [(< t  10) (format "0~a" t)]
          [(< t 100) (format "00~a" t)]
          [else (number->string t)])))

(define ~_time : (-> Integer String)
  (lambda [t]
    (if (< t 10)
        (format " ~a" t)
        (number->string t))))
