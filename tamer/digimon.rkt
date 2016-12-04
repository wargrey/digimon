#lang digimon

(require typed/racket/draw)

(use-compiled-file-paths)
(print-boolean-long-form)
(define-cheat-opaque bitmap%? #:is-a? Bitmap% bitmap%)
bitmap%?

(module+ test
  (define uuids : (HashTable String Integer) (make-hash))
  
  (for ([i (in-range 64)])
    (define uuid : String (uuid:timestamp))
    (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

  (for ([i (in-range 64)])
    (define uuid : String (uuid:random))
    (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

  (define errno : Natural
    (for/fold ([errno : Natural 0]) ([(uuid count) (in-hash uuids)])
      (displayln uuid)
      (if (= count 1) errno (add1 errno))))

  (unless (zero? errno)
    (printf "~a duplicates~n" errno)
    (exit errno)))

(module+ test
  (define ctx (make-EVP_MD_CTX))
  (for ([hash (in-list (list md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))]
        [hashname (in-list '(md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))])
    (define start (current-inexact-milliseconds))
    (printf "~a~n==> Hash: ~a~n==> HMAC: ~a~n~a~n" hashname
            (bytes->hex-string (HASH hash #""))
            (bytes->hex-string (HMAC hash #"" #""))
            (- (current-inexact-milliseconds) start)))
  (~EVP_MD_CTX ctx))

