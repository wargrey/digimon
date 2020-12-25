#lang typed/racket/base

(require digimon/archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pk.zip : Path (build-path (find-system-path 'temp-dir) "pk.zip"))
(define file:// : Path (collection-file-path "bintext" "digimon" "digitama"))

(define config#0 : (Listof ZIP-Deflation-Config) (list (zip-compression-preference 0)))
(define config#1 : (Listof ZIP-Deflation-Config) (list (zip-compression-preference 1)))
(define config#6 : (Listof ZIP-Deflation-Config) (list (zip-compression-preference 6)))
(define config#9 : (Listof ZIP-Deflation-Config) (list (zip-compression-preference 9)))

(define entries : (Listof Archive-Entry)
  (list (make-archive-file-entry (collection-file-path "." "digimon") "folder/digimon" #:methods '(stored))
        (make-archive-file-entry (collection-file-path "pkzip.rkt" "digimon" "tamer") "stored/pkzip.rkt" #:methods '(stored))
        (make-archive-ascii-entry #"stored ascii" "stored/ascii.txt" #:methods '(stored))
        (make-archive-binary-entry #"the stored data from stdin will be renamed randomly to stop `unzip` from reusing another entry's name" "" #:methods '(stored))

        (make-archive-binary-entry #"" "deflated/blank.λsh" #:methods '(deflated) #:options config#0)
        (make-archive-file-entry (build-path file:// "zipconfig.rkt") "deflated/zipconfig.rkt" #:methods '(deflated) #:options config#0)
        (make-archive-file-entry (build-path file:// "huffman.rkt") "deflated/huffman.rkt" #:methods '(deflated))))

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
