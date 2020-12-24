#lang typed/racket/base

(require digimon/archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pk.zip : Path (build-path (find-system-path 'temp-dir) "pk.zip"))

(define entries : (Listof Archive-Entry)
  (list (make-archive-file-entry (collection-file-path "tamer" "digimon") #:alt-name "folder/digimon/tamer" #:methods '(stored))
        (make-archive-file-entry (collection-file-path "pkzip.rkt" "digimon" "tamer") #:alt-name "stored/pkzip.rkt" #:methods '(stored))
        (make-archive-ascii-entry #"stored ascii" "stored/ascii.txt" #:methods '(stored))
        (make-archive-binary-entry #"the stored data from stdin will be renamed randomly to stop `unzip` from reusing another entry's name" "" #:methods '(stored))

        (make-archive-binary-entry #"stored block" "deflated/blank.csv" #:methods '(deflated))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/dtrace)
  
  (call-with-output-file* pk.zip #:exists 'replace
    (λ [[/dev/zipout : Output-Port]]
      (write pk.zip /dev/zipout)
      (zip-create /dev/zipout entries #:zip-root "pkzip")))

  (printf "Archive: ~a~n" pk.zip)

  (call-with-dtrace
      (λ []
        (zip-extract pk.zip)
        (newline))
    'trace))
