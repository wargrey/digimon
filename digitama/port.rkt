#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ports-filter-write : (->* ((Listof Output-Port) Bytes) (Index Index) (Listof Output-Port))
  (lambda [outs src [start 0] [end (bytes-length src)]]
    (reverse
     (for/fold ([outs++ : (Listof Output-Port) null])
               ([out (in-list outs)])
       (with-handlers ([exn:fail? (λ _ outs++)])
         (let copy ([s : Nonnegative-Fixnum start])
           (when (< s end)
             (copy (+ (write-bytes-avail/enable-break src out s end) s))))
         (flush-output out)
         (cons out outs++))))))
