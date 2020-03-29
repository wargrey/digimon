#lang typed/racket/base

(provide (all-defined-out))
(provide bplist? BPList BPList-Stdin)

(require "digitama/plist/bplist.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bplist : (-> BPList-Stdin BPList)
  (lambda [/dev/bplin]
    (bplist-from-bytes (bplist-stdin->bytes /dev/bplin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-pretty-print : (->* (BPList-Stdin) (Output-Port #:offset-table-column Byte #:show-unused-field? Boolean) Void)
  (lambda [/dev/bplin [/dev/stdout (current-output-port)] #:offset-table-column [offset-table-column 16] #:show-unused-field? [unused-field? #true]]
    (bplist-pretty-hexdump (bplist-stdin->bytes /dev/bplin) /dev/stdout offset-table-column unused-field?)))
