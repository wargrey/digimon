#lang typed/racket/base

(provide (all-defined-out))

(require (prefix-in base64: "digitama/bintext/base64.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define base64-encode : (-> (U String Bytes) [#:62nd Positive-Byte] [#:63rd Positive-Byte] Bytes)
  (lambda [/dev/stdin #:62nd [62nd base64:62nd] #:63rd [63rd base64:63rd]]
    (cond [(bytes? /dev/stdin) (base64:encode /dev/stdin 62nd 63rd)]
          [else (base64:encode (string->bytes/utf-8 /dev/stdin) 62nd 63rd)])))

(define base64-decode : (-> (U String Bytes) [#:62nd Positive-Byte] [#:63rd Positive-Byte] Bytes)
  (lambda [/dev/stdin #:62nd [62nd base64:62nd] #:63rd [63rd base64:63rd]]
    (cond [(bytes? /dev/stdin) (base64:decode /dev/stdin 62nd 63rd)]
          [else (base64:decode (string->bytes/utf-8 /dev/stdin) 62nd 63rd)])))
