#lang typed/racket/base

(provide (all-defined-out) iso-week-date)
(provide (all-from-out typed/racket/date))

(require typed/racket/date)
(require racket/fixnum)

(require "digitama/strftime.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Date-Format (U 'american 'chinese 'german 'indian 'irish 'iso-8601 'rfc2822 'julian))
(define-type Date-Origin (U 'utc 'locale 'zone))

(define-type Date-Datum (U date (Pairof Integer Date-Origin)
                           (List Integer Integer Integer)
                           (Vector Integer Integer Integer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s/min : Byte 60)
(define s/hour : Index 3600)
(define s/day : Index 86400)

(define floor-seconds : (-> Natural Natural Natural)
  (lambda [s span]
    (assert (- s (remainder s span)) exact-nonnegative-integer?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-microseconds : (-> Fixnum)
  (lambda []
    (fl->fx (real->double-flonum (* (current-inexact-milliseconds) 1000)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msdos-datetime->utc-seconds : (->* (Index Index) (Boolean) Natural)
  ; https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-dosdatetimetofiletime?redirectedfrom=MSDN
  (lambda [msdos-date msdos-time [locale? #false]]
    (define year (+ (bitwise-bit-field msdos-date 9 16) 1980))
    (define month (bitwise-bit-field msdos-date 5 9))
    (define day (bitwise-bit-field msdos-date 0 5))
    (define hour (bitwise-bit-field msdos-time 11 16))
    (define minute (bitwise-bit-field msdos-time 5 11))
    (define second (* (bitwise-bit-field msdos-time 0 5) 2))

    (assert (find-seconds (if (>= second 60) 59 second)
                          minute hour day month year locale?)
            exact-nonnegative-integer?)))

(define utc-seconds->msdos-datetime : (->* (Integer) (Boolean) (Values Index Index))
  ; https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-dosdatetimetofiletime?redirectedfrom=MSDN
  (lambda [utc-seconds [locale? #false]]
    (define the-date (seconds->date utc-seconds locale?))
    (define msdos-date : Natural
      (bitwise-ior (date-day the-date)
                   (arithmetic-shift (date-month the-date) 5)
                   (arithmetic-shift (max (- (date-year the-date) 1980) 0) 9)))
    (define future-second : Natural (quotient (+ (date-second the-date) 1) 2))
    (define msdos-time : Natural
      (bitwise-ior future-second
                   (arithmetic-shift (date-minute the-date) 5)
                   (arithmetic-shift (date-hour the-date) 11)))

    (cond [(= future-second 31) (utc-seconds->msdos-datetime (+ utc-seconds 1) locale?)]
          [else (values (assert msdos-date index?) (assert msdos-time index?))])))

(define time-zone-utc-bias-seconds : (-> Integer)
  (lambda []
    (date-time-zone-offset (seconds->date (current-seconds) #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define strftime : (->* ()
                        ((Option Date-Datum)
                         #:format (U Date-Format String) #:in (Option Symbol)
                         #:time? Boolean #:silent? Boolean)
                        String)
  (lambda [#:format [tfmt 'iso-8601] #:in [nation #false] #:time? [time? #true] #:silent? [silent? #true]
           [timepoint #false]]
    (define dt : date
      (cond [(not timepoint) (current-date)]
            [(date? timepoint) timepoint]
            [(list? timepoint) (seconds->date (find-seconds 0 0 0 (caddr timepoint) (cadr timepoint) (car timepoint) #false))]
            [(pair? timepoint) (seconds->date (car timepoint) (not (eq? (cdr timepoint) 'utc)))]
            [else (seconds->date (find-seconds 0 0 0 (vector-ref timepoint 2) (vector-ref timepoint 1) (vector-ref timepoint 0) #false))]))
    
    (if (string? tfmt)
        (let ([/dev/tmout : Output-Port (open-output-bytes)])
          (~strftime dt tfmt nation /dev/tmout silent?)
          (get-output-string /dev/tmout))
        (parameterize ([date-display-format tfmt])
          (date->string dt time?)))))
