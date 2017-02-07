#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4122, A Universally Unique IDentifier (UUID) URN Namespace    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (except-out (all-defined-out) variant+clock-sequence))
(provide (all-from-out typed/racket/random))

(require racket/math)
(require racket/format)
(require racket/fixnum)
(require racket/flonum)

(require typed/racket/random)

(define uuid:timestamp : (-> String)
  (lambda []
    (define version : Byte 1)
    (define no-ieee-mac-bit : Fixnum #x80000)
    (define utc:100ns : Integer (exact-round (fl* (current-inexact-milliseconds) 10000.0)))
    (define diff:1582-10-15 : Natural #x01B21DD213814000)
    (define ts : String (~a #:align 'right #:width 15 #:left-pad-string "0" (format "~x" (+ utc:100ns diff:1582-10-15))))
    (define time-low : String (substring ts 7 15))
    (define time-mid : String (substring ts 3 7))
    (define time-high : String (substring ts 0 3))
    (define pr+gc : Fixnum (current-process-milliseconds))
    (define gc : Fixnum (current-gc-milliseconds))
    (format "~a-~a-~a~a-~x-~a~a" time-low time-mid version time-high (variant+clock-sequence)
            (~a #:align 'right #:width 5 #:left-pad-string "0"
                (format "~x" (fxior no-ieee-mac-bit (fxand #xFFFF gc))))
            (~a #:align 'right #:width 7 #:left-pad-string "0"
                (format "~x" (fxand #xFFFFFFF pr+gc))))))

(define uuid:random : (-> String)
  (lambda []
    (define version : Byte 4)
    (define utc:us : Integer (exact-round (* (current-inexact-milliseconds) 1000)))
    (define ts : String (~a #:align 'right #:width 15 #:left-pad-string "0" (format "~x" utc:us)))
    (define time-low : String (substring ts 7 15))
    (define time-mid : String (substring ts 3 7))
    (define time-high : String (substring ts 0 3))
    (format "~a-~a-~a~a-~x-~a" time-low time-mid version time-high (variant+clock-sequence)
            (apply string-append (for/list : (Listof String) ([b (in-bytes (crypto-random-bytes 6))])
                                   (format (if (<= b #x0F) "0~x" "~x") b))))))

(define variant+clock-sequence : (-> Positive-Fixnum)
  (lambda [] ; TODO: what if the clock is set backwards
    (fxior #b1000000000000000 #| ensure the N is 8, 9, A, or B |#
           (fxand (current-memory-use) #b11111111111111))))
