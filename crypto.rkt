#lang typed/racket/base

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [EVP_MD_CTX_new evp-digest-context-create]
                       [EVP_MD_CTX_reset evp-digest-context-reset]))
  (provide (rename-out [EVP_get_digestbyname evp-digest-fetch]
                       [EVP_DigestUpdate evp-digest-update]
                       [EVP_MD_get_size evp-digest-size]
                       [EVP_MD_get_block_size evp-digest-block-size]))

  (require openssl/libcrypto)

  (require "ffi.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-cpointer-type _EVP_MD_CTX*)
  (define-cpointer-type _ENGINE*)
  (define-cpointer-type _EVP_MD*)

  (define-ffi-definer define-crypto libcrypto)

  (define-crypto EVP_MD_CTX_free (_fun _EVP_MD_CTX* -> _void) #:wrap (deallocator))
  (define-crypto EVP_MD_CTX_new (_fun -> _EVP_MD_CTX*) #:wrap (allocator EVP_MD_CTX_free))
  (define-crypto EVP_MD_CTX_reset (_fun _EVP_MD_CTX* -> _int))
  (define-crypto EVP_get_digestbyname (_fun _symbol -> _EVP_MD*/null))
  (define-crypto EVP_MD_get_size (_fun _EVP_MD* -> _int))
  (define-crypto EVP_MD_get_block_size (_fun _EVP_MD* -> _int))
  
  (define-crypto EVP_DigestInit_ex
    (_fun _EVP_MD_CTX* _EVP_MD* _ENGINE*/null
          -> [r : _int]
          -> (= r 1)))
  
  (define-crypto EVP_DigestUpdate
    (_fun _EVP_MD_CTX*
          [msg : _pointer]
          [_size = (bytes-length msg)]
          -> [r : _int] -> (= r 1)))
  
  (define-crypto EVP_DigestFinal_ex
    (_fun _EVP_MD_CTX*
          [msg : _pointer]
          [size : (_box _uint)]
          -> [r : _int]
          -> (= r 1)))

  ; this one will cleanup the CTX
  (define-crypto EVP_DigestFinal
    (_fun _EVP_MD_CTX*
          [msg : _pointer]
          [size : (_box _uint)]
          -> [r : _int]
          -> (= r 1)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define evp-digest
    (lambda [msg-digest messages digest [&size (box 0)]]
      (define mdctx (EVP_MD_CTX_new))
      
      (EVP_DigestInit_ex mdctx msg-digest #false)
      (evp-digest-update* mdctx messages)
      (EVP_DigestFinal_ex mdctx digest &size)

      (EVP_MD_CTX_free mdctx)))

  (define evp-digest-init
    (lambda [mdctx msg-digest [engine #false]]
      (EVP_DigestInit_ex mdctx msg-digest #false)))

  (define evp-digest-update*
    (lambda [mdctx messages]
      (for ([message (in-list messages)])
        (EVP_DigestUpdate mdctx message))))

  (define evp-digest-final
    (lambda [mdctx digest [&size (box 0)]]
      (EVP_DigestFinal mdctx digest &size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque EVP-MD* EVP_MD*?]
 [#:opaque EVP-MD-CTX* EVP_MD_CTX*?]
 [evp-digest-context-create (-> EVP-MD-CTX*)]
 [evp-digest-context-reset (-> EVP-MD-CTX* Integer)]
 [evp-digest-fetch (-> Symbol (Option EVP-MD*))]
 [evp-digest-init (-> EVP-MD-CTX* EVP-MD* Boolean)]
 [evp-digest-update (-> EVP-MD-CTX* Bytes Boolean)]
 [evp-digest-update* (-> EVP-MD-CTX* (Listof Bytes) Void)]
 [evp-digest-final (->* (EVP-MD-CTX* Bytes) ((Boxof Index)) Boolean)]
 [evp-digest (->* (EVP-MD* (Listof Bytes) Bytes) ((Boxof Index)) Void)]
 [evp-digest-size (-> EVP-MD* Index)]
 [evp-digest-block-size (-> EVP-MD* Index)])
