#lang typed/racket/base

(provide (all-defined-out))

(require "../debug.rkt")
(require "ioexn.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bytes->integer stx)
  (syntax-case stx [:]
    [(_ bytes->integer do-bytes->integer [argl : Argl] ... #:-> Integer_t)
     (syntax/loc stx
       (define bytes->integer : (All (a) (case-> [Bytes Integer Argl ... -> Integer_t]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) Symbol -> (∩ Integer_t a)]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) Symbol Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(src start argl ...) (do-bytes->integer src start argl ...)]
           [(src start argl ... subinteger?) (assert (do-bytes->integer src start argl ...) subinteger?)]
           [(src start argl ... subinteger? op) (assert* (do-bytes->integer src start argl ...) subinteger? throw-range-error op)]
           [(src start argl ... subinteger? op throw) (assert* (do-bytes->integer src start argl ...) subinteger? throw op)])))]
    [(_ bytes->integer do-bytes->integer signed? #:-> Integer_t)
     (syntax/loc stx
       (define bytes->integer : (All (a) (case-> [Bytes Integer -> Integer_t]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) Symbol -> (∩ Integer_t a)]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) Symbol Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(src start) (do-bytes->integer src start signed?)]
           [(src start subinteger?) (assert (do-bytes->integer src start signed?) subinteger?)]
           [(src start subinteger? op) (assert* (do-bytes->integer src start signed?) subinteger? throw-range-error op)]
           [(src start subinteger? op throw) (assert* (do-bytes->integer src start signed?) subinteger? throw op)])))]))

(define-syntax (define-bytes->integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer [bytes->integer #:-> Integer] [bytes->natural #:-> Natural]] ...)
     (syntax/loc stx
       (begin (define-bytes->integer bytes->integer do-bytes->integer #true  #:-> Integer) ...
              (define-bytes->integer bytes->natural do-bytes->integer #false #:-> Natural) ...))]))

(define-syntax (define-integer->bytes stx)
  (syntax-case stx [:]
    [(_ integer->bytes do-integer->bytes signed? msb? bsize)
     (syntax/loc stx
       (define integer->bytes : (->* (Integer) ((Option Bytes) Natural) Bytes)
         (lambda [n [bs #false] [offset 0]]
           (do-integer->bytes n bsize signed? msb? bs offset))))]))

(define-syntax (define-integer->bytes* stx)
  (syntax-case stx [:]
    [(_ do-integer->bytes msb? [bsize [integer->bytes] [natural->bytes]] ...)
     (syntax/loc stx
       (begin (define-integer->bytes integer->bytes do-integer->bytes #true  msb? bsize) ...
              (define-integer->bytes natural->bytes do-integer->bytes #false msb? bsize) ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (integer->msb-octets stx)
  (syntax-case stx [:]
    [(_ mpint #: (N bsize) #:-> bmpint #:at offset)
     (syntax/loc stx
       (let integer->octets ([sth : Nonnegative-Fixnum (+ bsize offset)]
                             [mpint : N mpint])
         (or (let ([sth-8 : Fixnum (- sth 8)])
               (and (>= sth-8 offset)
                    (integer->integer-bytes (bitwise-and mpint #xFFFFFFFFFFFFFFFF) 8 #false #true bmpint sth-8)
                    (integer->octets sth-8 (arithmetic-shift mpint -64))))
             (let ([sth-4 : Fixnum (- sth 4)])
               (and (>= sth-4 offset)
                    (integer->integer-bytes (bitwise-and mpint #xFFFFFFFF) 4 #false #true bmpint sth-4)
                    (integer->octets sth-4 (arithmetic-shift mpint -32))))
             (let ([sth-1 : Fixnum (- sth 1)])
               (and (>= sth-1 offset)
                    (bytes-set! bmpint sth-1 (bitwise-and mpint #xFF))
                    (integer->octets sth-1 (arithmetic-shift mpint -8))))
             
             bmpint)))]))

(define-syntax (msb-octets->integer stx)
  (syntax-case stx [:]
    [(_ bmpint #:from start #:to end #:-> N #:with x0)
     (syntax/loc stx
       (let octets->integer ([idx : Index (assert start index?)]
                             [x : N x0])
         (define idx+8 : Nonnegative-Fixnum (+ idx 8))
         (define idx+4 : Nonnegative-Fixnum (+ idx 4))
         (define idx+1 : Nonnegative-Fixnum (+ idx 1))
         
         (or (let ([idx+8 : Nonnegative-Fixnum (+ idx 8)])
               (and (<= idx+8 end)
                    (octets->integer idx+8 (bitwise-ior (arithmetic-shift x 64) (integer-bytes->integer bmpint #false #true idx idx+8)))))
             (let ([idx+4 : Nonnegative-Fixnum (+ idx 4)])
               (and (<= idx+4 end)
                    (octets->integer idx+4 (bitwise-ior (arithmetic-shift x 32) (integer-bytes->integer bmpint #false #true idx idx+4)))))
             (let ([idx+1 : Nonnegative-Fixnum (+ idx 1)])
               (and (<= idx+1 end)
                    (octets->integer idx+1 (bitwise-ior (arithmetic-shift x 8) (bytes-ref bmpint idx)))))

             x)))]))

(define-syntax (integer->lsb-octets stx)
  (syntax-case stx [:]
    [(_ mpint #: (N bsize) #:-> bmpint #:at offset)
     (syntax/loc stx
       (let ([end : Index (assert (+ bsize offset) index?)])
         (let integer->octets ([sth : Index offset]
                               [mpint : N mpint])
           (or (let ([sth+8 : Nonnegative-Fixnum (+ sth 8)])
                 (and (<= sth+8 end)
                      (integer->integer-bytes (bitwise-and mpint #xFFFFFFFFFFFFFFFF) 8 #false #false bmpint sth)
                      (integer->octets sth+8 (arithmetic-shift mpint -64))))
               (let ([sth+4 : Nonnegative-Fixnum (+ sth 4)])
                 (and (<= sth+4 end)
                      (integer->integer-bytes (bitwise-and mpint #xFFFFFFFF) 4 #false #false bmpint sth)
                      (integer->octets sth+4 (arithmetic-shift mpint -32))))
               (let ([sth+1 : Nonnegative-Fixnum (+ sth 1)])
                 (and (<= sth+1 end)
                      (bytes-set! bmpint sth (bitwise-and mpint #xFF))
                      (integer->octets sth+1 (arithmetic-shift mpint -8))))
               
               bmpint))))]))

(define-syntax (lsb-octets->integer stx)
  (syntax-case stx [:]
    [(_ bmpint #:from start #:to end #:-> N #:with x0)
     (syntax/loc stx
       (let octets->integer ([idx : Nonnegative-Fixnum end]
                             [x : N x0])
         (or (let ([idx-8 : Fixnum (- idx 8)])
               (and (>= idx-8 start)
                    (octets->integer idx-8 (bitwise-ior (arithmetic-shift x 64) (integer-bytes->integer bmpint #false #false idx-8 idx)))))
             (let ([idx-4 : Fixnum (- idx 4)])
               (and (>= idx-4 start)
                    (octets->integer idx-4 (bitwise-ior (arithmetic-shift x 32) (integer-bytes->integer bmpint #false #false idx-4 idx)))))
             (let ([idx-1 : Fixnum (- idx 1)])
               (and (>= idx-1 start)
                    (octets->integer idx-1 (bitwise-ior (arithmetic-shift x 8) (bytes-ref bmpint idx-1)))))

             x)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fixed-integer->bytes : (->* (Integer Integer Boolean Boolean) ((Option Bytes) Natural) Bytes)
  (lambda [n size signed? big-endian? [outbs #false] [offset0 0]]
    (case size
      [(1 2 4 8)
       (let-values ([(bs offset) (if (not outbs) (values (make-bytes size) 0) (values outbs offset0))])
         (integer->integer-bytes n size signed? big-endian? bs offset))]
      [else (or outbs #"")])))
