#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Syn-Token-Stdin PList-Stdin]))

(require racket/file)
(require racket/port)

(require "../token.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PList-Stdout (U Path-String Output-Port))

(define-type PList-Format (U 'bplist))
(define-type PList-Datum (U Void Boolean String Symbol Bytes Integer Flonum date))
(define-type PList-Object (Rec p (U PList-Datum (Listof p) (HashTable Symbol p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plist-stdin->bytes : (-> Syn-Token-Stdin Bytes)
  (lambda [/dev/bplin]
    (cond [(path? /dev/bplin) (file->bytes /dev/bplin)]
          [(string? /dev/bplin) (file->bytes /dev/bplin)]
          [(input-port? /dev/bplin) (port->bytes /dev/bplin)]
          [else /dev/bplin])))
