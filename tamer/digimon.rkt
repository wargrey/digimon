#lang digimon

(define-cheat-opaque bitmap%? #:is-a? (Class) object%)

(module+ test0
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

(module+ test0
  (define ctx (make-EVP_MD_CTX))
  (for ([hash (in-list (list md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))]
        [hashname (in-list '(md5 mdc2 dss dss1 ripemd160 sha1 sha224 sha256 sha384 sha512))])
    (define start (current-inexact-milliseconds))
    (printf "~a~n==> Hash: ~a~n==> HMAC: ~a~n~a~n" hashname
            (bytes->hex-string (HASH hash #""))
            (bytes->hex-string (HMAC hash #"" #""))
            (- (current-inexact-milliseconds) start)))
  (~EVP_MD_CTX ctx))

(module+ test0
  (require "../system.rkt")
  
  (for ([color (in-list '(grey red green blue yellow magenta cyan))])
    (define-values [darkcolor lightcolor] (values (format "dark~a" color) (format "light~a" color)))
    (echof "»»» 8/16 colors test:")
    (echof #:fgcolor color " ~a" color)
    (echof #:fgcolor darkcolor " ~a" darkcolor)
    (echof #:fgcolor lightcolor " ~a" lightcolor)
    (echof #:bgcolor color " ~a" color)
    (echof #:bgcolor darkcolor " ~a" darkcolor)
    (echof #:bgcolor lightcolor " ~a~n" lightcolor)
    (for ([effect (in-list '(bright dim underline blink reverse password))])
      (echof #:fgcolor darkcolor #:attributes (list effect) "dark:~a " effect)
      (echof #:fgcolor lightcolor #:attributes (list effect) "light:~a " effect))
    (newline))
  
  (echof "»»» 256 colors test:~n")
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:fgcolor (cast (sub1 color) Byte) caption)
    (when (zero? (remainder color 32))
      (newline)))
  
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:bgcolor (cast (sub1 color) Byte) caption)
    (when (zero? (remainder color 32))
      (newline))))

(module+ test
  (require "../tongue.rkt")

  (digimon-tongue-paths (list "wisemon" "nefertimon"))
  (all-tongues)
  (current-tongue)
  (speak 'Unknown #:in 'Tibetan)
  (speak 'Unknown))
