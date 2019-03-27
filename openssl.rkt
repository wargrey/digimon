#lang typed/racket

(provide (all-defined-out))

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/port)
  
  (require (for-syntax racket/base))
  (require (for-syntax racket/syntax))
  
  (require "digitama/openssl.rkt")

  (define-syntax (define-crypto-md stx)
    (syntax-case stx []
      [(_ id ...)
       (with-syntax ([(racket-id ...) (for/list ([md-name (in-list (syntax->list #'(id ...)))])
                                        (datum->syntax md-name (format-id #'racket-id "evp-~a" (syntax-e md-name))))])
         #'(begin (define racket-id (EVP_get_digestbyname 'id)) ...))]))

  (define openctx (make-EVP_MD_CTX))
  
  (define HASH
    (lambda [md-name . messages]
      (define md (EVP_get_digestbyname md-name))

      (define-crypto-md sha1 sha224 sha256)
      
      (EVP_DigestInit_ex openctx md)
      
      (for ([datum (in-list messages)])
        (define message
          (cond [(bytes? datum) datum]
                [(string? datum) (string->bytes/utf-8 datum)]
                [(input-port? datum) (port->bytes datum)]
                [else (with-output-to-bytes (λ [] (write datum)))]))
        
        (EVP_DigestUpdate openctx message))
      
      (EVP_DigestFinal_ex openctx))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [HASH (-> Symbol Any * Bytes)])
