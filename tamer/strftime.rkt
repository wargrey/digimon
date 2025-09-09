#lang typed/racket/base

(require digimon/date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define strftime-exec : (-> (Option Natural) Date-Origin Void)
  (lambda [s origin]
    (define tp (and s (cons s origin)))
    
    (printf "timepoint ~a(~a): ~a~n" s origin (strftime tp))
    
    (printf "1. Custom format strings:~n")
    (define common-formats
      '("%Y-%m-%d %H:%M:%S"         ; Standard datetime
        "%A, %B %d, %Y"             ; Full date
        "%Y年%m月%d日"               ; Chinese date format (output will be Chinese)
        "Timestamp: %s"             ; Unix timestamp
        "Week %V of %G"             ; ISO week info
        "%FT%T%z"                   ; ISO8601 with timezone
        ))
  
    (for ([fmt common-formats])
      (printf "   \"~a\" => ~a~n" fmt
              (strftime tp #:format fmt)))
    
    (printf "2. Localization test:~n")
    (define locales '(#f en en_US en_GB zh ja))
    (define localized-formats
      '("%+"
        "%x %X"
        "%A %B %d %Y - %H:%M"))

    (for ([fmt localized-formats])
      (printf "   ~s~n" fmt)
      (for ([lang locales])
        (printf "     Locale ~a: ~a~n" 
                lang 
                (strftime tp #:format fmt #:in lang))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(strftime-exec 1704067200 'utc) ; 2024-01-01 00:00:00 UTC
(strftime-exec 0 'utc)
(strftime-exec 0 'zone)
