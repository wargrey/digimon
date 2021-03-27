#lang typed/racket/base

(require digimon/checksum)
(require digimon/dtrace)
(require digimon/format)
(require digimon/debug)

(require digimon/digitama/bintext/zip)
(require digimon/digitama/bintext/lz77)
(require digimon/digitama/bintext/deflation)

(require "lz77.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define deflation-run : LZ77-Run
  (lambda [txt strategies min-match farthest filtered winbits memlevel]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))

    (printf "[window size: ~a] [memory level: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a]~n"
            (~size (arithmetic-shift 1 winbits)) memlevel (or min-match 'auto) farthest filtered)

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
           (λ [] (let ([&csize : (Boxof Natural) (box 0)])
                   (let ([/dev/zipout (open-output-deflated-block #:window-bits winbits #:memory-level memlevel
                                                                  #:name desc #:fixed-only? (not (lz77-dynamic))
                                                                  /dev/bsout strategy #false)])
                     (write-bytes-avail* txt /dev/zipout)
                     (close-output-port /dev/zipout)
                     (set-box! &csize (file-position /dev/bsout)))
                   
                   (let ([/dev/zipin (open-input-deflated-block (open-input-bytes (get-output-bytes /dev/bsout #true)) #false #true #:name desc)])
                     (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e))])
                       (let ([?errmsg (zip-entry-copy/trap /dev/zipin /dev/bsout rsize (checksum-crc32 txt 0 rsize))])
                         (when (string? ?errmsg) (dtrace-error ?errmsg))
                         (custodian-shutdown-all (current-custodian)))))
                   
                   (assert (unbox &csize) index?)))))
        
        (let ([?txt (get-output-bytes /dev/bsout #true)])
          (lz77-display-summary desc txt ?txt csize rsize widths memory cpu real gc))))
    
    (when (and (not (lz77-verbose)) (lz77-brief))
      (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (lz77-main (current-command-line-arguments) deflation-run))
