#lang typed/racket/base

(module+ main
  (require digimon/dtrace)
  (require digimon/archive)
  
  (require "pkzip.rkt")
  
  (call-with-output-file* pk.zip #:exists 'replace
    (λ [[/dev/zipout : Output-Port]]
      (write pk.zip /dev/zipout)
      (zip-create #:zip-root "pkzip" #:memory-level memlevel
                  /dev/zipout entries)))

  (printf "Archive: ~a~n" pk.zip)

  (call-with-dtrace
      (λ [] (zip-extract pk.zip (make-archive-verification-reader #:dtrace-topic 'unzip-t)))
    'trace))
