#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax stdio-referenced-field))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (stdio-referenced-field <field> <fields>)
  (define field (syntax-e <field>))
  (unless (memq field (syntax->datum <fields>))
    (raise-syntax-error 'define-file-header "undefined field" <field>))
  <field>)

(define-syntax (call-datum-reader stx)
  (syntax-parse stx #:datum-literals []
    [(_ read-datum n:nat /dev/stdin self sizes #false)
     #'(read-datum /dev/stdin)]
    [(_ read-datum n:nat /dev/stdin self sizes #true)
     #'(let ([v (read-datum /dev/stdin)]) (hash-set! sizes self v) v)]
    [(_ read-datum field:id /dev/stdin self sizes fields ...)
     #'(read-datum /dev/stdin (hash-ref sizes 'field))]))

(define-syntax (integer-size-for-writer stx)
  (syntax-parse stx #:datum-literals []
    [(_ n:nat) #'n]
    [_ #'0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-integer-bytes! : (-> Input-Port Natural Bytes)
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdin size]
      (define s : (U Natural EOF) (read-bytes! intptr /dev/stdin 0 size))
      (cond [(eq? s size) intptr]
            [else (throw-eof-error /dev/stdin)]))))

(define write-fixed-integer : (-> Output-Port Integer Integer Boolean Boolean Byte)
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdout n size signed? big-endian?]
      (case size
        [(1) (write-byte n /dev/stdout) 1]
        [(2) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 2) 2]
        [(4) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 4) 4]
        [(8) (integer->integer-bytes n size signed? big-endian? intptr 0) (write-bytes intptr /dev/stdout 0 8) 8]
        [else 0]))))
