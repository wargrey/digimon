#lang typed/racket/base

(module+ main
  (require digimon/debug)
  (require digimon/dtrace)
  (require digimon/archive)
  
  (require typed/racket/date)
  
  (require "pkzip.rkt")

  (date-display-format 'iso-8601)
  
  (time** (call-with-output-file* pk.zip #:exists 'replace
            (λ [[/dev/zipout : Output-Port]]
              (write pk.zip /dev/zipout)
              (zip-create #:zip-root "pkunzip" #:memory-level memlevel
                          /dev/zipout entries))))

  (printf "Archive: ~a~n" pk.zip)

  (call-with-dtrace
      (λ [] (zip-extract pk.zip (make-archive-filesystem-reader 'temp-dir 32 #:checksum? #true)))
    'trace))
