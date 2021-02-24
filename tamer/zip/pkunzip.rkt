#lang typed/racket/base

(module+ main
  (require digimon/debug)
  (require digimon/dtrace)
  (require digimon/archive)
  
  (require typed/racket/date)
  
  (require "pkzip.rkt")

  (collect-garbage*)

  (date-display-format 'iso-8601)
  
  (time-apply*
   (λ [] (call-with-output-file* pk.zip #:exists 'replace
           (λ [[/dev/zipout : Output-Port]]
             (write pk.zip /dev/zipout)
             (zip-create #:zip-root "pkzip" #:memory-level memlevel
                         /dev/zipout entries))))
   #true)

  (printf "Archive: ~a~n" pk.zip)

  (call-with-dtrace
      (λ [] #;(zip-extract pk.zip (make-archive-verification-reader #:dtrace '|unzip -t|))
        (zip-extract pk.zip (make-archive-filesystem-reader 'temp-dir 32 #:checksum? #true)))
    'trace))
