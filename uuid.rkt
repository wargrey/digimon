#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4122, A Universally Unique IDentifier (UUID) URN Namespace    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (all-from-out typed/racket/random))

(require racket/math)
(require racket/format)
(require racket/fixnum)
(require racket/flonum)
(require racket/string)

(require typed/racket/random)

(define (uuid:timestamp) : String (uuid->string (uuid#timestamp)))
(define (uuid:random) : String (uuid->string (uuid#random)))

(define uuid#timestamp : (-> Integer)
  (lambda []
    (define version : Byte #b0001)
    (define variant2 : Byte #b10 #|ensure N is 8 9 A or B|#)
    (define no-ieee-mac-bit : Fixnum #b1)
    (define utc:100ns : Integer (exact-round (fl* (current-inexact-milliseconds) 10000.0)))
    (define diff:1582-10-15 : Natural #x01B21DD213814000)
    (define ts60 : Integer (+ utc:100ns diff:1582-10-15))
    (define time-low32 : Integer  (bitwise-and ts60 #xFFFFFFFF))
    (define time-mid16 : Integer  (bitwise-and (arithmetic-shift ts60 -32) #xFFFF))
    (define time-high12 : Integer (bitwise-and (arithmetic-shift ts60 -48) #x0FFF))
    (define clock14 : Integer (fxand (current-process-milliseconds) #b11111111111111 #|16383ms|#))
    (define random-node47:17 : Integer (fxand (current-gc-milliseconds) #x1FFFF #|131071ms|#))
    (define random-node47:30 : Fixnum (fxand (current-memory-use) #x3FFFFFFF #|1GB|#))
    (bitwise-ior (arithmetic-shift time-low32 96)
                 (arithmetic-shift time-mid16 80)
                 (arithmetic-shift version 76)
                 (arithmetic-shift time-high12 64)
                 (arithmetic-shift variant2 62)
                 (arithmetic-shift clock14 48)
                 (arithmetic-shift no-ieee-mac-bit 47)
                 (arithmetic-shift random-node47:17 30)
                 random-node47:30)))

(define uuid#random : (-> Integer)
  (lambda []
    (define version : Byte #b0100)
    (define variant2 : Byte #b10 #|ensure N is 8 9 A or B|#)
    (define no-ieee-mac-bit : Fixnum #b1)
    (define time-low32 : Integer (integer-bytes->integer (crypto-random-bytes 4) #false #true 0 4))
    (define time-mid16 : Integer (integer-bytes->integer (crypto-random-bytes 2) #false #true 0 2))
    (define time-high12 : Integer (random (fx+ #x0FFF 1)))
    (define clock14 : Integer (random (fx+ #b11111111111111 1)))
    (define node47:15 : Integer (random (fx+ #x7FFF 1)))
    (define node47:32 : Integer (integer-bytes->integer (crypto-random-bytes 4) #false #true 0 4))
    (bitwise-ior (arithmetic-shift time-low32 96)
                 (arithmetic-shift time-mid16 80)
                 (arithmetic-shift version 76)
                 (arithmetic-shift time-high12 64)
                 (arithmetic-shift variant2 62)
                 (arithmetic-shift clock14 48)
                 (arithmetic-shift no-ieee-mac-bit 47)
                 (arithmetic-shift node47:15 32)
                 node47:32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uuid->string : (-> Integer String)
  (lambda [uuid]
    (define raw : String (~a #:align 'right #:width 32 #:left-pad-string "0" (number->string uuid 16)))
    (string-append (substring raw 0 8) "-"
                   (substring raw 8 12) "-"
                   (substring raw 12 16) "-"
                   (substring raw 16 20) "-"
                   (substring raw 20 32))))

(define string->uuid : (-> String Integer)
  (lambda [uuid]
    (define uuid? : (Option Number) (string->number (string-replace uuid "-" "") 16))
    (if (exact-integer? uuid?) uuid? 0)))
