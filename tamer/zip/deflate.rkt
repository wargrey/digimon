#lang typed/racket/base

(require racket/port)

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
  (lambda [txt strategies bits min-match farthest filtered memory-level]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))

    (when (lz77-verbose)
      (printf "[hash Bits: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a] [memory level: ~a]~n"
              bits (or min-match 'auto) farthest filtered memory-level))

    (define widths : (Listof Index)
      (text-column-widths (list (list "T" "strategy-name" "000.000KB" "100.00%"
                                      "00.000" "00.000" "00.000" "000.000KB"))))

    (for ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
      (define desc : String (strategy->description strategy))
      
      (when (lz77-verbose) (printf ">>> [strategy: ~a]~n" desc))

      (parameterize ([current-custodian (make-custodian)])
        (define /dev/bsout (open-output-bytes))

        (define-values (csize memory cpu real gc)
          (time-apply**
           (λ []
             (let ([&csize : (Boxof Natural) (box 0)])
               (let ([/dev/zipout (open-output-deflated-block #:memory-level memory-level #:name desc #:allow-dynamic-block? (lz77-dynamic)
                                                              /dev/bsout strategy #false)])
                 (write-bytes-avail txt /dev/zipout)
                 (close-output-port /dev/zipout)
                 (set-box! &csize (file-position /dev/bsout)))
               
               (let ([/dev/zipin (open-input-deflated-block (open-input-bytes (get-output-bytes /dev/bsout #true)) 0 #true #:name desc)])
                 (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e))])
                   (let-values ([(csize crc32) (zip-entry-copy /dev/zipin /dev/bsout)])
                     (custodian-shutdown-all (current-custodian)))))
               
               (assert (unbox &csize) index?)))))
        
        (let ([?txt (get-output-bytes /dev/bsout #true)])
          (lz77-display-summary desc txt ?txt csize rsize widths memory cpu real gc))))

    (when (and (not (lz77-verbose)) (lz77-brief))
      (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (lz77-main (current-command-line-arguments) deflation-run))
