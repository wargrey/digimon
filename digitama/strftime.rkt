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
                          [%? : Boolean #false]
                          [ext-pad : (Option String) #false])
      (if (< idx size)
          (let ([ch (string-ref fmt idx)])
            (cond [(and %?)
                   (let ([pad++ (case/eq ch
                                  [(#\A) (write-string (~weekday/full (date-week-day self) nation) /dev/tmout)]
                                  [(#\a) (write-string (~weekday/abbr (date-week-day self) nation) /dev/tmout)]
                                  [(#\B) (write-string (~month/full (date-month self) nation) /dev/tmout)]
                                  [(#\b) (write-string (~month/abbr (date-month self) nation) /dev/tmout)]
                                  [(#\C) (write-string (~pad (quotient (date-year self) 100) "0" ext-pad) /dev/tmout)]
                                  [(#\c) (~strftime self (~localize '%c nation) nation /dev/tmout)]
                                  [(#\D) (~strftime self "%m/%d/%y" nation /dev/tmout #true)]
                                  [(#\d) (write-string (~pad (date-day self) "0" ext-pad) /dev/tmout)]
                                  [(#\e) (write-string (~pad (date-day self) " " ext-pad) /dev/tmout)]
                                  [(#\F) (~strftime self "%Y-%m-%d" nation /dev/tmout #true)]
                                  [(#\G) (write (~year/iso self) /dev/tmout)] ; for %V year-week, monday first
                                  [(#\g) (write-string (~pad (remainder (~year/iso self) 100) "0" ext-pad) /dev/tmout)] ; same as above
                                  [(#\H) (write-string (~pad (date-hour self) "0" ext-pad) /dev/tmout)] ; [0, 23]
                                  [(#\h) (~strftime self "%b" nation /dev/tmout #true)]
                                  [(#\I) (write-string (~pad (~hour12 (date-hour self)) "0" ext-pad) /dev/tmout)]
                                  [(#\j) (write-string (~0year (add1 (date-year-day self)) ext-pad) /dev/tmout)]
                                  [(#\k) (write-string (~pad (date-hour self) " " ext-pad) /dev/tmout)]
                                  [(#\l) (write-string (~pad (~hour12 (date-hour self)) " " ext-pad) /dev/tmout)]
                                  [(#\M) (write-string (~pad (date-minute self) "0" ext-pad) /dev/tmout)]
                                  [(#\m) (write-string (~pad (date-month self) "0" ext-pad) /dev/tmout)]
                                  [(#\n) (newline /dev/tmout)]
                                  [(#\P) (write-string (string-downcase (~am/pm (date-hour self) nation)) /dev/tmout)]
                                  [(#\p) (write-string (string-upcase (~am/pm (date-hour self) nation)) /dev/tmout)]
                                  [(#\R) (~strftime self "%H:%M" nation /dev/tmout #true)]
                                  [(#\r) (~strftime self "%I:%M:%S %p" nation /dev/tmout #true)]
                                  [(#\S) (write-string (~pad (date-second self) "0" ext-pad) /dev/tmout)]
                                  [(#\s) (write (date->seconds self #false) /dev/tmout)]
                                  [(#\T) (~strftime self "%H:%M:%S" nation /dev/tmout #true)]
                                  [(#\t) (write-char #\t /dev/tmout)]
                                  [(#\U) (write-string (~pad (~old-week/sunday self) "0" ext-pad) /dev/tmout)] ; Sunday first, [00, 53]
                                  [(#\u) (write (~weekday/sunday (date-week-day self)) /dev/tmout)] ; Monday first, [1, 7]
                                  [(#\V) (write-string (~pad (~week/iso self) "0" ext-pad) /dev/tmout)] ; Monday first, [01, 53]
                                  [(#\v) (~strftime self "%e-%b-%Y" nation /dev/tmout #true)]
                                  [(#\W) (write-string (~pad (~old-week/monday self) "0" ext-pad) /dev/tmout)] ; Monday first, [00, 53]
                                  [(#\w) (write (date-week-day self) /dev/tmout)] ; Sunday first, [0, 6]
                                  [(#\X) (~strftime self (~localize '%X nation) nation /dev/tmout)]
                                  [(#\x) (~strftime self (~localize '%x nation) nation /dev/tmout)]
                                  [(#\Y) (write (date-year self) /dev/tmout)]
                                  [(#\y) (write-string (~pad (remainder (date-year self) 100) "0" ext-pad) /dev/tmout)]
                                  [(#\Z) (write-string (date*-time-zone-name (assert self date*?)) /dev/tmout)]
                                  [(#\z) (write-string (~tz-offset self ext-pad) /dev/tmout)]
                                  [(#\+) (~strftime self "%a %b %d %H:%M:%S %Z %Y" nation /dev/tmout)]
                                  [(#\%) (write-char #\% /dev/tmout)]
                                  [(#\-) ""] [(#\_) " "] [(#\0) "0"]
                                  [else (and (not silent?)
                                             (raise-syntax-error 'strftime
                                                                 (format "unrecognized conversion specification@~a" idx)
                                                                 '|format string|
                                                                 (datum->syntax #false fmt
                                                                                (vector #false 1 idx (+ idx 1) 1))))])])
                     (if (string? pad++)
                         (subftime fmt (+ idx 1) #true pad++)
                         (subftime fmt (+ idx 1) #false #false)))]
                  [(eq? ch #\%) (subftime fmt (+ idx 1) #true #false)]
                  [else (write-char ch /dev/tmout) (subftime fmt (+ idx 1) #false #false)]))
          (when (and %? (not silent?))
            (raise-syntax-error 'strftime "terminated with %" 'format))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~year/iso : (-> date Integer)
  (lambda [d]
    (define-values (year week)  (iso-week-date d))
    year))

(define ~week/iso : (-> date Integer)
  (lambda [d]
    (define-values (year week)  (iso-week-date d))
    week))

;;; https://webspace.science.uu.nl/~gent0113/calendar/isocalendar.htm
(define week-day-of-1231 : (-> Integer Integer)
  (lambda [y]
    (remainder (+ y
                  (quotient y 4)
                  (- (quotient y 100))
                  (quotient y 400))
               7)))

(define weeks-of-year : (-> Integer Byte)
  (lambda [y]
    (if (or #;#:|ends Thursday|   (= (week-day-of-1231 y) 4)
            #;#:|starts Thursday| (= (week-day-of-1231 (- y 1)) 3))
        53
        52)))

(define iso-week-date : (-> date (Values Natural Byte))
  (lambda [d]
    (define y (date-year d))

    ; w = (10 + year-day - week-day) div 7
    ; year-day <- [1, 366]
    ; week-day <- [1, 7], Monday first
    (define w (quotient (+ 11 (date-year-day d) (- (~weekday/sunday (date-week-day d)))) 7))

    (cond [(and (<= 1 w) (<= w 52)) (values y w)] ; hot path
          [(< w 1) (values (assert (- y 1) exact-nonnegative-integer?) (weeks-of-year (- y 1)))]
          [(> w (weeks-of-year y)) (values (+ y 1) 1)]
          [else (values y w)])))

(define ~old-week/sunday : (-> date Integer)
  (lambda [d]
    (quotient (+ (date-year-day d) 7 (- (date-week-day d)))
              7)))

(define ~old-week/monday : (-> date Integer)
  (lambda [d]
    (quotient (+ (date-year-day d) 6 (- (~weekday/sunday (date-week-day d))))
              7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~tz-offset : (-> date (Option String) String)
  (lambda [d user-request-padding]
    (define delta : Integer (date-time-zone-offset d))
    (define hours : Integer (quotient delta 3600))
    (define minutes : Integer (modulo (quotient delta 60) 60))

    (string-append (if (negative? delta) "-" "+")
                   (~pad (abs hours) "0" user-request-padding)
                   (~pad minutes "0" #false))))

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

(define ~0year : (-> Integer (Option String) String)
  (lambda [t user-request-padding]
    (define pad (or user-request-padding "0"))
    
    (cond [(< t  10) (string-append pad pad (number->string t))]
          [(< t 100) (string-append pad (number->string t))]
          [else (number->string t)])))

(define ~pad : (-> Integer String (Option String) String)
  (lambda [t pad user-request-padding]
    (if (< t 10)
        (string-append (or user-request-padding pad) (number->string t))
        (number->string t))))
