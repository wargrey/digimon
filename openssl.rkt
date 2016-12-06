#lang typed/racket

(provide (all-defined-out) bytes->hex-string hex-string->bytes)
(provide (all-from-out typed/openssl))

(require (submod "digitama/ffi.rkt" typed))

(require typed/openssl)
(require (only-in typed/openssl/sha1 bytes->hex-string hex-string->bytes))

(require/typed/provide/pointers
 "digitama/openssl.rkt"
 [EVP_MD_CTX* EVP_MD_CTX*?]
 [EVP_MD* EVP_MD*?]
 [ENGINE* ENGINE*?])

(require/typed/provide/batch
 "digitama/openssl.rkt"
 (id: md5 mdc2 ripemd160 dss dss1
      sha1 sha224 sha256 sha384 sha512)
 EVP_MD*)

(require/typed/provide
 "digitama/openssl.rkt"
 [openssl-lib-versions (Listof String)]
 [EVP_get_digestbyname (-> Symbol (Option EVP_MD*))]
 [make-EVP_MD_CTX (-> EVP_MD_CTX*)]
 [~EVP_MD_CTX (-> EVP_MD_CTX* Void)]
 [EVP_DigestInit_ex (-> EVP_MD_CTX* EVP_MD* Boolean)]
 [EVP_DigestUpdate (-> EVP_MD_CTX* Bytes Boolean)]
 [EVP_DigestFinal_ex (-> EVP_MD_CTX* Bytes)]
 [HMAC (-> EVP_MD* Bytes Bytes Bytes)])

(define openctx : EVP_MD_CTX* (make-EVP_MD_CTX))
(define HASH : (-> EVP_MD* Any * Bytes)
  (lambda [md . messages]
    (EVP_DigestInit_ex openctx md)
    (for ([datum (in-list messages)])
      (define message : Bytes
        (cond [(bytes? datum) datum]
              [(string? datum) (string->bytes/utf-8 datum)]
              [(input-port? datum) (port->bytes datum)]
              [else (with-output-to-bytes (thunk (write datum)))]))
      (EVP_DigestUpdate openctx message))
    (EVP_DigestFinal_ex openctx)))
