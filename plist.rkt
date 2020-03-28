#lang typed/racket/base

(provide (all-defined-out))
(provide plist? PList PList-Stdin)

(require "digitama/plist.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-plist : (-> PList-Stdin PList)
  (lambda [/dev/plstin]
    (plist-from-bytes (plist-stdin->bytes /dev/plstin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-pretty-print : (->* (PList-Stdin) (Output-Port) Void)
  (lambda [/dev/plstin [/dev/stdout (current-output-port)]]
    (plist-pretty-hexdump (plist-stdin->bytes /dev/plstin) /dev/stdout)))
