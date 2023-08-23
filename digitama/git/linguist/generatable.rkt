#lang typed/racket/base

(provide (all-defined-out))

(require "../../../filesystem.rkt")

(require racket/string)

;;; https://github.com/github/linguist/tree/master/lib/linguist/documentation.yml
;;; https://github.com/github/linguist/tree/master/lib/linguist/vendor.yml

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-reader read-pathname-regexps #:+ (Listof PRegexp)
  (lambda [/dev/ymlin src]
    (let regexp-fold ([regexps : (Listof PRegexp) null])
      (define line (read-line /dev/ymlin 'any))

      (cond [(not (string? line)) (reverse regexps)]
            [(<= (string-length line) 1) (regexp-fold regexps)]
            [(not (eq? (string-ref line 0) #\-)) (regexp-fold regexps)]
            [else (regexp-fold (cons (pregexp (string-trim (substring line 1))) regexps))]))))
