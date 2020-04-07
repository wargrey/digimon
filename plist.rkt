#lang typed/racket/base

(provide (all-defined-out))
(provide PList-Datum PList-Stdin)

(require "digitama/plist.rkt")
(require "digitama/plist/bplist.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-plist : (-> PList-Stdin PList-Datum)
  (lambda [/dev/bplin]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))
    (cond [(bplist-bytes? body) (bplist-extract-object body)]
          [else (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-dissect : (->* (PList-Stdin) (Output-Port #:offset-table-column Byte #:show-unused-field? Boolean) Void)
  (lambda [/dev/bplin [/dev/stdout (current-output-port)] #:offset-table-column [offset-table-column 16] #:show-unused-field? [unused-field? #true]]
    (define body : Bytes (plist-stdin->bytes /dev/bplin))

    (when (bplist-bytes? body)
      (bplist-pretty-hexdump body /dev/stdout offset-table-column unused-field?))))
