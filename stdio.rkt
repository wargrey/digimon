#lang typed/racket/base

(provide (all-defined-out))

(require "debug.rkt")

(require "digitama/ioexn.rkt")
(require "digitama/number.rkt")

(require "digitama/unsafe/number.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Subbytes (List Bytes Index Index))
(define-type Octets (U Bytes Subbytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define read-n:bytes : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (read-nbytes* /dev/stdin (read-msize /dev/stdin size))))

(define-read-integer* read-nbytes*
  [msb-bytes->octet 1 [read-mint8  #:-> Fixnum]  [read-muint8  #:-> Byte]]
  [msb-bytes->short 2 [read-mint16 #:-> Fixnum]  [read-muint16 #:-> Index]]
  [msb-bytes->int   4 [read-mint32 #:-> Fixnum]  [read-muint32 #:-> Index]]
  [msb-bytes->long  8 [read-mint64 #:-> Integer] [read-muint64 #:-> Natural]])

(define-read-integer* read-nbytes*
  [lsb-bytes->octet 1 [read-lint8  #:-> Fixnum]  [read-luint8  #:-> Byte]]
  [lsb-bytes->short 2 [read-lint16 #:-> Fixnum]  [read-luint16 #:-> Index]]
  [lsb-bytes->int   4 [read-lint32 #:-> Fixnum]  [read-luint32 #:-> Index]]
  [lsb-bytes->long  8 [read-lint64 #:-> Integer] [read-luint64 #:-> Natural]])

(define-read-integer read-msize msb-bytes->index read-nbytes* #:-> Index)
(define-read-integer read-lsize lsb-bytes->index read-nbytes* #:-> Index)

(define read-signature : (-> Input-Port Bytes Boolean)
  (lambda [/dev/stdin signature]
    (define siglength : Index (bytes-length signature))
    
    (and (equal? signature (peek-nbytes* /dev/stdin siglength))
         (read-bytes siglength /dev/stdin)
         #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bytes* : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (cond [(bytes? bs) bs]
          [(raise (throw-eof-error /dev/stdin))])))

(define read-nbytes* : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin)])))

(define peek-bytes* : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (if (bytes? bs) bs (throw-eof-error /dev/stdin))))

(define peek-nbytes* : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin)])))
