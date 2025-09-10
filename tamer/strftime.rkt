#lang typed/racket/base

(require digimon/date)
(require digimon/string)

(require digimon/spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-iso-date year month day iso)
  #:it ["~a => ~s" (strftime (list year month day) #:format "%a %-d %b %Y(%F)") iso] #:do
  (expect-string= (strftime (list year month day) #:format "%G-W%V-%u") iso))

(define-behavior (it-check-special-iso-date year month day iso reason)
  #:it ["~a: ~a => ~s" reason (strftime (list year month day) #:format "%a %-d %b %Y(%F)") iso] #:do
  (expect-string= (strftime (list year month day) #:format "%G-W%V-%u") iso))

(define-feature prelude #:do
  (describe "Week Number" #:do
    (describe "ISO 8601 Week Date (%G & %V)" #:do
      (context "Contemporary Dates around New Year's Day" #:do
        (it-check-iso-date 2005 01 01 "2004-W53-6")
        (it-check-iso-date 2005 01 02 "2004-W53-7")
        (it-check-iso-date 2005 12 31 "2005-W52-6")
        
        (it-check-iso-date 2006 01 01 "2005-W52-7")
        (it-check-iso-date 2006 01 02 "2006-W01-1")
        (it-check-iso-date 2006 12 31 "2006-W52-7")
        
        (it-check-iso-date 2007 12 30 "2007-W52-7")
        (it-check-iso-date 2007 12 31 "2008-W01-1")
        
        (it-check-iso-date 2010 01 01 "2009-W53-5")
        (it-check-iso-date 2010 01 02 "2009-W53-6")
        (it-check-iso-date 2010 01 03 "2009-W53-7")

        (context "Special Years" #:do
          (it-check-special-iso-date 1970 01 01 "1970-W01-4" 'Epoch)
          (it-check-special-iso-date 2000 01 01 "1999-W52-6" 'Millennium)
          (it-check-special-iso-date 2007 01 01 "2007-W01-1" '|ISO Aligned|)
          (it-check-special-iso-date 2009 01 01 "2009-W01-4" '|Thursday Start|)
          (it-check-special-iso-date 2009 12 31 "2009-W53-4" '|53-Week Year|))
        
        (context "Leap Years" #:do
          (it-check-iso-date 2004 02 29 "2004-W09-7")
          (it-check-iso-date 2000 12 31 "2000-W52-7")

          ; ISO 2008 is 2 days shorter
          (it-check-iso-date 2008 01 01 "2008-W01-2")
          (it-check-iso-date 2008 12 28 "2008-W52-7")
          (it-check-iso-date 2008 12 29 "2009-W01-1")
          (it-check-iso-date 2008 12 30 "2009-W01-2")
          (it-check-iso-date 2008 12 31 "2009-W01-3"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define strftime-zone : (-> (Option Natural) Date-Origin Void)
  (lambda [s origin]
    (define tp (and s (cons s origin)))
    
    (printf "timepoint ~a(~a): ~a~n" s origin (strftime tp))
    
    (define formats
      '("Unix Timestamp: %s"
        "ISO Week Info: %G-W%V-%u"
        "Old Week Info: %U starts on Sunday"
        "Old Week Info: %W starts on Monday"
        "%FT%T%z"))

    (for ([fmt formats])
      (printf "   ~a => ~a~n" fmt
              (strftime #:format fmt
                        tp)))

    (newline)))

(define strftime-localize : (-> (Option Natural) Date-Origin Void)
  (lambda [s origin]
    (define tp (and s (cons s origin)))
    
    (printf "Localization test for timepoint ~a(~a): ~a~n" s origin (strftime tp))
    
    (define locales '(#f en en_US en_GB zh ja))
    (define localized-formats
      '("%+"
        "%c"
        "%x %X"
        "%A %B %d %Y - %H:%M"))

    (for ([fmt localized-formats])
      (printf "   ~s~n" fmt)
      (for ([lang locales])
        (printf "     Locale ~a: ~a~n" lang 
                (strftime tp #:format fmt #:in lang)))
      (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main 
  (strftime-zone 0 'utc)
  (strftime-zone 0 'zone)
  (strftime-zone 1704067200 'utc)
  (strftime-zone #false 'utc)
  
  (strftime-localize 1704067200 'utc)
  (strftime-localize #false 'utc)

  (void (spec-prove prelude)))
