#lang typed/racket/base

(provide (all-defined-out))
(provide PList-Datum PList-Format)
(provide PList-Stdin PList-Stdout)

(require "digitama/plist.rkt")
(require "digitama/plist/bplist.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-plist : (-> PList-Stdin PList-Datum)
  (lambda [/dev/bplin]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))
    (cond [(bplist-bytes? body) (bplist-extract-object body)]
          [else (void)])))

(define write-plist : (->* (PList-Datum) (PList-Stdout #:format PList-Format #:exists (U 'error 'replace)) Void)
  (lambda [plst [/dev/bplout (current-output-port)] #:format [type 'bplist] #:exists [exists 'replace]]
    (if (output-port? /dev/bplout)
        (case type
          [else (bplist-write-datum plst /dev/bplout)])
        ((inst call-with-output-file* Void)
         /dev/bplout #:exists exists #:mode 'binary
         (λ [[/dev/bplout : Output-Port]]
           (write-plist plst /dev/bplout))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-dissect : (->* (PList-Stdin) (Output-Port #:offset-table-column Byte #:show-unused-field? Boolean) Void)
  (lambda [/dev/bplin [/dev/stdout (current-output-port)] #:offset-table-column [offset-table-column 16] #:show-unused-field? [unused-field? #true]]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))

    (when (bplist-bytes? body)
      (bplist-pretty-hexdump body /dev/stdout offset-table-column unused-field?))))
