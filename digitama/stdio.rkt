#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require "../debug.rkt")
(require "ioexn.rkt")

(require "unsafe/number.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-read-integer stx)
  (syntax-case stx [:]
    [(_ read-integer do-bytes->integer #:-> Integer_t)
     #'(define read-integer : (All (a) (case-> [Input-Port Natural -> Integer_t]
                                               [Input-Port Natural (-> Any Boolean : a) -> a]
                                               [Input-Port Natural (-> Any Boolean : a) Symbol -> a]
                                               [Input-Port Natural (-> Any Boolean : a) Symbol Throw-Range-Error -> a]))
         (case-lambda
           [(/dev/stdin size) (do-bytes->integer (read-integer-bytes! /dev/stdin size) 0 size)]
           [(/dev/stdin size subinteger?) (assert (read-integer /dev/stdin size) subinteger?)]
           [(/dev/stdin size subinteger? op) (read-integer /dev/stdin size subinteger? op throw-range-error)]
           [(/dev/stdin size subinteger? op throw) (assert* (read-integer /dev/stdin size) subinteger? throw op)]))]
    [(_ read-integer do-bytes->integer signed? bsize #:-> Integer_t)
     #'(define read-integer : (All (a) (case-> [Input-Port -> Integer_t]
                                               [Input-Port (-> Any Boolean : a) -> a]
                                               [Input-Port (-> Any Boolean : a) Symbol -> a]
                                               [Input-Port (-> Any Boolean : a) Symbol Throw-Range-Error -> a]))
         (case-lambda
           [(/dev/stdin) (do-bytes->integer (read-integer-bytes! /dev/stdin bsize) 0 signed?)]
           [(/dev/stdin subinteger?) (assert (read-integer /dev/stdin) subinteger?)]
           [(/dev/stdin subinteger? op) (read-integer /dev/stdin subinteger? op throw-range-error)]
           [(/dev/stdin subinteger? op throw) (assert* (read-integer /dev/stdin) subinteger? throw op)]))]))

(define-syntax (define-read-integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer bsize [read-integer #:-> Integer] [read-natural #:-> Natural]] ...)
     #'(begin (define-read-integer read-integer do-bytes->integer #true  bsize #:-> Integer) ...
              (define-read-integer read-natural do-bytes->integer #false bsize #:-> Natural) ...)]))

(define-syntax (define-peek-integer stx)
  (syntax-case stx [:]
    [(_ peek-integer do-bytes->integer #:-> Integer_t)
     #'(define peek-integer : (->* (Input-Port Natural) (Natural) (Option Integer_t))
         (lambda [/dev/stdin size [skip 0]]
           (define ?intptr (peek-integer-bytes! /dev/stdin size skip))
           (and ?intptr (do-bytes->integer ?intptr 0 size))))]
    [(_ peek-integer do-bytes->integer signed? bsize #:-> Integer_t)
     #'(define peek-integer : (->* (Input-Port) (Natural) (Option Integer_t))
         (lambda [/dev/stdin [skip 0]]
           (define ?intptr (peek-integer-bytes! /dev/stdin bsize skip))
           (and ?intptr (do-bytes->integer ?intptr 0 signed?))))]))

(define-syntax (define-peek-integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer bsize [peek-integer #:-> Integer] [peek-natural #:-> Natural]] ...)
     #'(begin (define-peek-integer peek-integer do-bytes->integer #true  bsize #:-> Integer) ...
              (define-peek-integer peek-natural do-bytes->integer #false bsize #:-> Natural) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (stdio-target-field <field> <fields>)
  (define field (syntax-e <field>))
  (unless (memq field (syntax->datum <fields>))
    (raise-syntax-error 'define-file-header "undefined field" <field>))
  <field>)

(define-for-syntax (stdio-word-size <n>)
  (define n (syntax-e <n>))
  (unless (memq n (list 1 2 4 8))
    (raise-syntax-error 'define-file-header "invalid size" <n>))
  (datum->syntax <n> (- n)))

(define-syntax (call-datum-reader stx)
  (syntax-parse stx #:datum-literals []
    [(_ read-datum n:nat /dev/stdin self sizes #false)
     #'(read-datum /dev/stdin)]
    [(_ read-datum n:nat /dev/stdin self sizes #true)
     #'(let ([v (read-datum /dev/stdin)]) (hash-set! sizes self v) v)]
    [(_ read-datum field:id /dev/stdin self sizes fields ...)
     #'(read-datum /dev/stdin (hash-ref sizes 'field))]
    [(_ read-datum n:integer /dev/stdin self sizes fields ...)
     #'(read-datum /dev/stdin (- n))]))

(define-syntax (call-datum-reader* stx)
  (syntax-parse stx #:datum-literals []
    [(_ signature [] read-datum ...) #'(check-signature signature (call-datum-reader read-datum ...))]
    [(_ signature [raw->datum args ...] read-datum ...) #'(raw->datum (check-signature signature (call-datum-reader read-datum ...)) args ...)]))

(define-syntax (call-datum-writer stx)
  (syntax-parse stx #:datum-literals []
    [(_ [] write-datum field-value n /dev/stdout)
     #'(write-datum field-value (integer-size-for-writer n) /dev/stdout)]
    [(_ [datum->raw args ...] write-datum field-value n /dev/stdout)
     #'(write-datum (datum->raw field-value args ...) (integer-size-for-writer n) /dev/stdout)]))

(define-syntax (integer-size-for-writer stx)
  (syntax-parse stx #:datum-literals []
    [(_ n:nat) #'n]
    [(_ n:integer) #'(- n)]
    [_ #'0]))

(define-syntax (check-signature stx)
  (syntax-parse stx #:datum-literals []
    [(_ [#false who-cares ...] datum) #'datum]
    [(_ [#true /dev/stdin src magic-number] datum) #'(stdio-signature-filter /dev/stdin datum magic-number 'src)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-integer-bytes! : (-> Input-Port Natural Bytes)
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdin size]
      (define s : (U Natural EOF) (read-bytes! intptr /dev/stdin 0 size))
      (cond [(eq? s size) intptr]
            [else (throw-eof-error /dev/stdin)]))))

(define peek-integer-bytes! : (-> Input-Port Natural Natural (Option Bytes))
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdin size skip]
      (define s : (U Natural EOF) (peek-bytes! intptr skip /dev/stdin 0 size))
      (and (eq? s size) intptr))))

(define write-fixed-integer : (-> Output-Port Integer Integer Boolean Boolean Byte)
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdout n size signed? big-endian?]
      (case size
        [(1) (write-byte n /dev/stdout) 1]
        [(2) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 2) 2]
        [(4) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 4) 4]
        [(8) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 8) 8]
        [else 0]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stdio-fixed-size : (-> Any Zero)
  (lambda [_]
    0))

(define stdio-signature-filter : (All (a) (-> Input-Port a Any Symbol a))
  (lambda [/dev/stdin given expected src]
    (unless (eq? given expected)
      (throw-signature-error /dev/stdin src "signature mismatched"))

    given))
