#lang typed/racket/base

(require digimon/archive)

(require digimon/digitama/bintext/huffman)
(require digimon/digitama/bintext/table/huffman)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pk.zip : Path (build-path (find-system-path 'temp-dir) "pk.zip"))
(define file:// : Path (collection-file-path "bintext" "digimon" "digitama"))
(define tamer:// : Path (collection-file-path "zip" "digimon" "tamer"))

(define config#0 : (Listof Any)  (list 0))
(define config#1 : (Listof Any)  (list 1))
(define config#6 : (Listof Any)  (list 6))
(define config#9 : (Listof Any)  (list 9))
(define config#id : (Listof Any) (list 'huffman-only))

(define entries : Archive-Entries
  (list (make-archive-file-entry (collection-file-path "." "digimon") "folder/digimon" #:methods '(stored))
        (make-archive-file-entry (collection-file-path "pkzip.rkt" "digimon" "tamer" "zip") "stored/pkzip.rkt" #:methods '(stored))
        (make-archive-ascii-entry #"stored ascii" "stored/ascii.txt" #:methods '(stored))
        (make-archive-binary-entry #"the stored data from stdin will be renamed randomly to stop `unzip` from reusing another entry's name" "" #:methods '(stored))

        (make-archive-binary-entry #"" "deflated/blank.λsh" #:methods '(deflated) #:options config#0)
        (make-archive-binary-entry #"these data haven't been compressed by LZ77" "deflated/fixed/identity.λsh" #:methods '(deflated) #:options config#id)
        (make-archive-binary-entry #"Fa-la-la-la-la" "deflated/fixed/overlap.λsh" #:methods '(deflated) #:options (list 6 'fixed))

        (for/list : (Listof Archive-Entry) ([n (in-range 3 upcodes)])
          (let ([bs (string->bytes/utf-8 (apply string (build-list n integer->char)))])
            (make-archive-binary-entry #:methods '(deflated) #:options (list 6 'fixed)
                                       (bytes-append bs bs) (format "deflated/fixed/backref/~a.λsh" n))))

        (for/list : (Listof Archive-Entry) ([base (in-vector huffman-distance-bases)]
                                            [extra (in-vector huffman-distance-extra-bits)]
                                            [idx (in-naturals)])
          (let ([bs (make-bytes (+ base upcodes extra) (+ 65 extra))])
            (make-archive-binary-entry #:methods '(deflated) #:options (list (zip-run-preference base) 'fixed)
                                       bs (format "deflated/fixed/backref/~a:~a.λsh" idx base))))
        
        (make-archive-file-entry (build-path file:// "zipconfig.rkt") "deflated/config#0.rkt" #:methods '(deflated) #:options config#0)
        (make-archive-file-entry (build-path file:// "huffman.rkt") "deflated/config#1.rkt" #:methods '(deflated) #:options config#1)
        (make-archive-file-entry (build-path file:// "lz77.rkt") "deflated/config#9.rkt" #:methods '(deflated) #:options config#9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/dtrace)
  
  (call-with-output-file* pk.zip #:exists 'replace
    (λ [[/dev/zipout : Output-Port]]
      (write pk.zip /dev/zipout)
      (zip-create /dev/zipout entries #:zip-root "pkzip")))

  (printf "Archive: ~a~n" pk.zip)

  #;(call-with-dtrace
      (λ []
        (zip-extract pk.zip)
        (newline))
    'trace))
