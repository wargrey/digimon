#lang digimon/ffi

(require "../openssl.rkt")

(define ctx (make-EVP_MD_CTX))
(for ([hash (in-list (list md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))]
      [hashname (in-list '(md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))])
  (define start (current-inexact-milliseconds))
  (printf "~a~n==> Hash: ~a~n==> HMAC: ~a~n~a~n" hashname
          (bytes->hex-string (HASH hash #""))
          (bytes->hex-string (HMAC hash #"" #""))
          (- (current-inexact-milliseconds) start)))
(~EVP_MD_CTX ctx)
