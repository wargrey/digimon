#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PList-Stdin (U Path-String Bytes))
(define-type PList-Stdout (U Path-String Output-Port))

(define-type PList-Format (U 'bplist))
(define-type PList-Datum (U Void Boolean String Symbol Bytes Integer Flonum date))
(define-type PList-Object (Rec p (U PList-Datum (Listof p) (HashTable Symbol p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-stdin->bytes : (-> PList-Stdin Bytes)
  (lambda [/dev/bplin]
    (cond [(path? /dev/bplin) (file->bytes /dev/bplin)]
          [(string? /dev/bplin) (file->bytes /dev/bplin)]
          [else /dev/bplin])))
