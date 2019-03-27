#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.openssl.org/docs/manmaster/crypto/evp.html                                       ;;;
;;; https://www.openssl.org/docs/manmaster/crypto/EVP_DigestInit_ex.html                         ;;;
;;; https://www.openssl.org/docs/manmaster/crypto/hmac.html                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require openssl)
(require openssl/libcrypto)

(when (string? ssl-load-fail-reason) (error 'openssl "~a~n" ssl-load-fail-reason))
(when (string? libcrypto-load-fail-reason) (error 'openssl "~a~n" libcrypto-load-fail-reason))

(define-ffi-definer define-crypto libcrypto)
(define EVP-SIZE-UPTO-SHA512 64)

;;; HASH Functions
(define-cpointer-type _EVP_MD_CTX*)
(define-cpointer-type _EVP_MD*)
(define-cpointer-type _ENGINE*)

(define-crypto EVP_get_digestbyname
  (_fun _symbol -> _EVP_MD*/null))

(define-crypto ~EVP_MD_CTX
  (_fun _EVP_MD_CTX* -> _void)
  #:c-id EVP_MD_CTX_free     ; before libcrypto 1.1, it is EVP_MD_CTX_destroy
  #:wrap (deallocator))

(define-crypto make-EVP_MD_CTX
  (_fun -> _EVP_MD_CTX*/null)
  #:c-id EVP_MD_CTX_new      ; before libcrypto 1.1, it is EVP_MD_CTX_create
  #:wrap (allocator ~EVP_MD_CTX))

(define-crypto EVP_DigestInit_ex
  (_fun [ctx : _EVP_MD_CTX*]
        [type : _EVP_MD*]
        [engine : _ENGINE*/null = #false]
        -> [non-standard-retcode : _int]
        -> (not (zero? non-standard-retcode))))

(define-crypto EVP_DigestUpdate
  (_fun [ctx : _EVP_MD_CTX*]
        [message : _bytes]
        [cnt : _size = (bytes-length message)]
        -> [non-standard-retcode : _int]
        -> (not (zero? non-standard-retcode))))

(define-crypto EVP_DigestFinal_ex
  (_fun [ctx : _EVP_MD_CTX*]
        [digest : (_bytes o EVP-SIZE-UPTO-SHA512)]
        [size : (_ptr o _uint)]
        -> [non-standard-retcode : _int]
        -> (and (= non-standard-retcode 1)
                (make-sized-byte-string digest size))))

;;; HMAC Functions
(define-crypto HMAC
  (_fun [type : _EVP_MD*]
        [key : _bytes]
        [key-size : _size = (bytes-length key)]
        [data : _bytes]
        [data-size : _size = (bytes-length data)]
        [mac : (_bytes o EVP-SIZE-UPTO-SHA512)]
        [size : (_ptr o _uint)]
        -> _bytes
        -> (make-sized-byte-string mac size)))
