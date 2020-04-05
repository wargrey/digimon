#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PList-Stdin (U Path-String Bytes))
(define-type PList-Datum (Rec p (U Void Boolean String Bytes Integer Flonum date (Immutable-Vectorof p) (Immutable-HashTable Symbol p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-stdin->bytes : (-> PList-Stdin Bytes)
  (lambda [/dev/bplin]
    (cond [(path? /dev/bplin) (file->bytes /dev/bplin)]
          [(string? /dev/bplin) (file->bytes /dev/bplin)]
          [else /dev/bplin])))
