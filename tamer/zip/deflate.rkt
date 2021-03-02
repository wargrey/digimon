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
  (lambda [txt strategies bits min-match farthest filtered]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))

    (printf "[size: ~a] [hash Bits: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a]~n"
            (~size (bytes-length txt)) bits (or min-match 'auto) farthest filtered)

    (define widths : (Listof Index)
      (text-column-widths (list (list "T" "strategy-name" "000.000KB" "100.00%"
                                      "00.000" "00.000" "00.000" "000.000KB"))))

    (for ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
      (define desc : String (strategy->description strategy))
      
      (when (lz77-verbose) (printf ">>> [strategy: ~a]~n" desc))

      (parameterize ([current-custodian (make-custodian)])
        (define-values (/dev/lzin /dev/lzout) (make-pipe-with-specials))
        (define-values (/dev/bitin /dev/bitout) (make-pipe))
        (define /dev/bsout (open-output-bytes))
        
        (define enqueue : LZ77-Submit-Symbol
          (case-lambda
            [(sym d-idx)
             (write-special (box sym) /dev/lzout)
             (when (lz77-verbose)
               (echof "~a " sym #:fgcolor 'blue))]
            [(distance span d-idx)
             (write-special (cons distance span) /dev/lzout)
             (when (lz77-verbose)
               (echof "~a " (cons distance span) #:fgcolor 'blue))]))

        (define dequeue : LZ77-Submit-Symbol
          (case-lambda
            [(sym d-idx)
             (let ([osym (read-byte-or-special /dev/lzin)])
               (define okay? (and (box? osym) (eq? sym (unbox osym))))
               (when (lz77-verbose)
                 (echof "~a " sym #:fgcolor (if okay? 'yellow 'red))))]
            [(distance span d-idx)
             (let ([osym (read-byte-or-special /dev/lzin)])
               (define okay? (and (pair? osym) (eq? distance (car osym)) (eq? span (cdr osym))))
               (when (lz77-verbose)
                 (echof "~a " (cons distance span) #:fgcolor (if okay? 'yellow 'red))))]))
        
        (define-values (/dev/zipin /dev/zipout)
          (values (open-input-deflated-block /dev/bitin 0 #true #:name desc #:lz77-hook dequeue)
                  (open-output-deflated-block #:memory-level 1 #:name desc #:lz77-hook enqueue
                                              /dev/bitout strategy #false)))

        (define-values (csize memory cpu real gc)
          (time-apply** (λ [] (let ([&csize : (Boxof Natural) (box 0)])
                                (thread (λ [] (begin (write-bytes-avail txt /dev/zipout)
                                                     (close-output-port /dev/zipout)
                                                     (set-box! &csize (file-position /dev/bitout))
                                                     (close-output-port /dev/bitout))))

                                (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e))])
                                  (let-values ([(csize crc32) (zip-entry-copy /dev/zipin /dev/bsout)])
                                    (custodian-shutdown-all (current-custodian))))

                                (assert (unbox &csize) index?)))))

        (let ([?txt (get-output-bytes /dev/bsout #true)])
          (lz77-display-summary desc txt ?txt csize rsize widths memory cpu real gc)
          (newline))

        #;(when (pair? errdata)
          (displayln errdata))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (lz77-main (current-command-line-arguments) deflation-run))
