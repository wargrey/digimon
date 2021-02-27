#lang typed/racket/base

(require digimon/thread)
(require digimon/dtrace)
(require digimon/format)
(require digimon/number)
(require digimon/debug)
(require digimon/echo)

(require digimon/digitama/bintext/zip)
(require digimon/digitama/bintext/lz77)
(require digimon/digitama/bintext/deflate)

(require "lz77.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define deflation-run : LZ77-Run
  (lambda [txt strategies bits min-match farthest filtered]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))

    (printf "[size: ~a] [hash Bits: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a]~n"
            (~size (bytes-length txt)) bits (or min-match 'auto) farthest filtered)

    (define widths : (Listof Index)
      (text-column-widths (list (list "T" "strategy-name" "00.000KB" "100.00%"
                                      "00.000" "00.000" "00.000" "00.000KB"))))

    (for ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
      (define desc : String (strategy->description strategy))
      
      (when (lz77-verbose) (printf ">>> [strategy: ~a]~n" desc))

      (parameterize ([current-custodian (make-custodian)])
        (define-values (/dev/bitin /dev/bitout) (make-pipe))
        (define /dev/bsout (open-output-bytes))

        (define-values (/dev/zipin /dev/zipout)
          (values (open-input-deflated-block /dev/bitin 0 #true #:name desc)
                  (open-output-deflated-block #:allow-dynamic-block? #false #:memory-level 1 #:name desc #:safe-flush-on-close? #true
                                              /dev/bitout strategy #true)))

        (define-values (csize memory cpu real gc)
          (time-apply** (λ [] (thread
                               (λ [] (begin (write-bytes txt /dev/zipout)
                                            (close-output-port /dev/zipout))))
        
                          (let-values ([(csize crc32) (zip-entry-copy /dev/zipin /dev/bsout)])
                            (custodian-shutdown-all (current-custodian))
                            (assert csize index?)))))

        (let ([?txt (get-output-bytes /dev/bsout #true)])
          (lz77-display-summary desc txt ?txt csize rsize widths memory cpu real gc)
          (newline))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (lz77-main (current-command-line-arguments) deflation-run))
