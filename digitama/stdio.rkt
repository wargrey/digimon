#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/format)
(require racket/fixnum)
(require racket/string)
(require racket/symbol)

(require "../debug.rkt")
(require "ioexn.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-read-integer stx)
  (syntax-case stx [:]
    [(_ read-integer do-bytes->integer #:-> Integer_t)
     (syntax/loc stx
       (define read-integer : (All (a) (case-> [Input-Port Natural -> Integer_t]
                                               [Input-Port Natural (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                               [Input-Port Natural (-> Any Boolean : (∩ Integer_t a)) (U Symbol Procedure) -> (∩ Integer_t a)]
                                               [Input-Port Natural (-> Any Boolean : (∩ Integer_t a)) (U Symbol Procedure) Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(/dev/stdin size) (do-bytes->integer (read-integer-bytes! /dev/stdin size) 0 size)]
           [(/dev/stdin size subinteger?) (assert (read-integer /dev/stdin size) subinteger?)]
           [(/dev/stdin size subinteger? op) (assert* (read-integer /dev/stdin size) subinteger? throw-range-error /dev/stdin op)]
           [(/dev/stdin size subinteger? op throw) (assert* (read-integer /dev/stdin size) subinteger? throw /dev/stdin op)])))]
    [(_ read-integer do-bytes->integer signed? bsize #:-> Integer_t)
     (syntax/loc stx
       (define read-integer : (All (a) (case-> [Input-Port -> Integer_t]
                                               [Input-Port (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                               [Input-Port (-> Any Boolean : (∩ Integer_t a)) (U Symbol Procedure) -> (∩ Integer_t a)]
                                               [Input-Port (-> Any Boolean : (∩ Integer_t a)) (U Symbol Procedure) Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(/dev/stdin) (do-bytes->integer (read-integer-bytes! /dev/stdin bsize) 0 signed?)]
           [(/dev/stdin subinteger?) (assert (read-integer /dev/stdin) subinteger?)]
           [(/dev/stdin subinteger? op) (assert* (read-integer /dev/stdin) subinteger? throw-range-error /dev/stdin op)]
           [(/dev/stdin subinteger? op throw) (assert* (read-integer /dev/stdin) subinteger? throw /dev/stdin op)])))]))

(define-syntax (define-read-integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer bsize [read-integer #:-> Integer] [read-natural #:-> Natural]] ...)
     (syntax/loc stx
       (begin (define-read-integer read-integer do-bytes->integer #true  bsize #:-> Integer) ...
              (define-read-integer read-natural do-bytes->integer #false bsize #:-> Natural) ...))]))

(define-syntax (define-write-integer stx)
  (syntax-case stx [:]
    [(_ write-integer do-write signed? msb? bsize)
     (syntax/loc stx
       (define write-integer : (->* (Integer) (Output-Port) Byte)
         (lambda [n [/dev/stdout (current-output-port)]]
           (do-write /dev/stdout n bsize signed? msb?))))]))

(define-syntax (define-write-integer* stx)
  (syntax-case stx [:]
    [(_ do-write msb? [bsize [write-integer] [write-natural]] ...)
     (syntax/loc stx
       (begin (define-write-integer write-integer do-write #true  msb? bsize) ...
              (define-write-integer write-natural do-write #false msb? bsize) ...))]))

(define-syntax (define-peek-integer stx)
  (syntax-case stx [:]
    [(_ peek-integer do-bytes->integer #:-> Integer_t)
     (syntax/loc stx
       (define peek-integer : (->* (Input-Port Natural) (Natural) (Option Integer_t))
         (lambda [/dev/stdin size [skip 0]]
           (define ?intptr (peek-integer-bytes! /dev/stdin size skip))
           (and ?intptr (do-bytes->integer ?intptr 0 size)))))]
    [(_ peek-integer do-bytes->integer signed? bsize #:-> Integer_t)
     (syntax/loc stx
       (define peek-integer : (->* (Input-Port) (Natural) (Option Integer_t))
         (lambda [/dev/stdin [skip 0]]
           (define ?intptr (peek-integer-bytes! /dev/stdin bsize skip))
           (and ?intptr (do-bytes->integer ?intptr 0 signed?)))))]))

(define-syntax (define-peek-integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer bsize [peek-integer #:-> Integer] [peek-natural #:-> Natural]] ...)
     (syntax/loc stx
       (begin (define-peek-integer peek-integer do-bytes->integer #true  bsize #:-> Integer) ...
              (define-peek-integer peek-natural do-bytes->integer #false bsize #:-> Natural) ...))]))

(define-syntax (define-limited-unicode-reader stx)
  (syntax-case stx [:]
    [(_ id #:->* iTypes oTypes Type #:lambda [args ...] #:-> ->datum sexp ...)
     (with-syntax ([id* (datum->syntax #'id (string->symbol (format "~a*" (syntax-e #'id))))])
       (syntax/loc stx
         (begin (define id : (->* iTypes oTypes (Values Type Byte))
                  (lambda [args ...]
                    (define-values (unicode other) (begin sexp ...))
                    (values (->datum unicode) other)))

                (define id* : (->* iTypes oTypes Type)
                  (lambda [args ...]
                    (define-values (unicode _) (begin sexp ...))
                    (->datum unicode))))))]
    [(_ id #:->* iTypes oTypes Type #:lambda [args ...] sexp ...)
     (with-syntax ([id* (datum->syntax #'id (string->symbol (format "~a*" (syntax-e #'id))))])
       (syntax/loc stx
         (begin (define id : (->* iTypes oTypes (Values Type Byte))
                  (lambda [args ...]
                    sexp ...))

                (define id* : (->* iTypes oTypes Type)
                  (lambda [args ...]
                    (define-values (unicode _) (begin sexp ...))
                    unicode)))))]))

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
    [(_ read-datum n:nat /dev/stdin self sizes #false args ...)
     (syntax/loc stx (read-datum /dev/stdin args ...))]
    [(_ read-datum n:nat /dev/stdin self sizes #true args ...)
     (syntax/loc stx (let ([v (read-datum /dev/stdin args ...)]) (hash-set! sizes self v) v))]
    [(_ read-datum field:id /dev/stdin self sizes fields ...)
     (syntax/loc stx (read-datum /dev/stdin (hash-ref sizes 'field)))]
    [(_ read-datum n:integer /dev/stdin self sizes fields ...)
     (syntax/loc stx (read-datum /dev/stdin (- n)))]))

(define-syntax (call-datum-reader* stx)
  (syntax-parse stx #:datum-literals [assert]
    [(_ signature [] read-datum ...)
     (syntax/loc stx (check-signature signature (call-datum-reader read-datum ...)))]
    [(_ signature [[subint?] args ...] read-datum ...)
     (syntax/loc stx (check-signature signature (call-datum-reader read-datum ... subint? args ...)))]
    [(_ signature [raw->datum args ...] read-datum ...)
     (syntax/loc stx (raw->datum (check-signature signature (call-datum-reader read-datum ...)) args ...))]))

(define-syntax (call-datum-writer stx)
  (syntax-parse stx #:datum-literals []
    [(_ [] write-datum field-value n /dev/stdout)
     (syntax/loc stx (write-datum field-value (integer-size-for-writer n) /dev/stdout))]
    [(_ [datum->raw args ...] write-datum field-value n /dev/stdout)
     (syntax/loc stx (write-datum (datum->raw field-value args ...) (integer-size-for-writer n) /dev/stdout))]))

(define-syntax (call-datum-writer* stx)
  (syntax-parse stx #:datum-literals []
    [(_ #false force? s-expr ...) (syntax/loc stx (call-datum-writer s-expr ...))]
    [(_ #true force? s-expr ...) (syntax/loc stx (if (not force?) 0 (call-datum-writer s-expr ...)))]))

(define-syntax (integer-size-for-writer stx)
  (syntax-parse stx #:datum-literals []
    [(_ n:nat) #'n]
    [(_ n:integer) (syntax/loc stx (- n))]
    [_ #'0]))

(define-syntax (check-signature stx)
  (syntax-parse stx #:datum-literals []
    [(_ [#false who-cares ...] datum-expr) #'datum-expr]
    [(_ [#true #false peek-sig /dev/stdin src magic-number] datum-expr) (syntax/loc stx (stdio-signature-filter /dev/stdin datum-expr magic-number 'src))]
    [(_ [#true #true  peek-sig /dev/stdin src magic-number] datum-expr) (syntax/loc stx (if (eq? magic-number (peek-sig /dev/stdin)) datum-expr magic-number))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-integer-bytes! : (-> Input-Port Natural Bytes)
  (let ([intptr (make-bytes 8)])
    (lambda [/dev/stdin size]
      (define s : (U Natural EOF) (read-bytes! intptr /dev/stdin 0 size))
      (cond [(eq? s size) intptr]
            [else (throw-eof-error /dev/stdin 'read-integer-bytes!)]))))

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

(define stdio-signature-filter : (All (a) (-> Input-Port (∩ a Number) (∩ a Number) Symbol a))
  (lambda [/dev/stdin given expected src]
    (unless (eq? given expected)
      (throw-signature-error /dev/stdin src "signature mismatched (expect: ~a; given: ~a)"
                             (number->string expected 16) (number->string given 16)))

    given))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stdio-field->name : (-> Symbol String)
  (lambda [field]
    (string-trim (string-replace (symbol->immutable-string field) #px"[-_]+" " "))))

(define stdio-select-writer : (-> (U Zero One Boolean) (-> Any Output-Port Void))
  (lambda [mode]
    (case mode
      [(#true) write]
      [(#false) display]
      [else (λ [[datum : Any] [/dev/stdout : Output-Port]] (print datum /dev/stdout mode))])))

(define stdio-write-field : (->* (Any Any Integer) ((-> Any Output-Port Void) Output-Port) Void)
  (lambda [datum size radix [write-datum display] [/dev/stdout (current-output-port)]]
    (or (and (exact-nonnegative-integer? datum)
             (exact-nonnegative-integer? size)
             (case radix
               [(2)  (fprintf /dev/stdout "#b~a" (~r datum #:base radix #:min-width (* size 8) #:pad-string "0"))]
               [(8)  (fprintf /dev/stdout "#o~a" (~r datum #:base radix #:min-width (quotient (* size 8) 3) #:pad-string "0"))]
               [(16) (fprintf /dev/stdout "#x~a" (~r datum #:base (list 'up radix) #:min-width (* size 2) #:pad-string "0"))]
               [else #false]))
        (write-datum datum /dev/stdout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define drop-bytes : (-> Input-Port Natural Void)
  (let* ([pool-size 4096]
         [/dev/null (make-bytes pool-size)])
    (lambda [/dev/stdin n]
      (let drop ([n : Integer n])
        (when (> n 0)
          (define count : (U Natural EOF) (read-bytes! /dev/null /dev/stdin 0 n))
          (unless (eof-object? count)
            (drop (- n count))))))))

(define read-unlimited-decimal : (-> Input-Port Byte (-> Char Boolean) (-> Char Index) Natural Natural Boolean Natural)
  (lambda [/dev/stdin bitwidth char-digit? char->decimal skip initial-result eat-last-whitespace?]
    (when (> skip 0)
      (drop-bytes /dev/stdin skip))
 
    (let read-octal ([skip : Natural 0]
                     [result : Natural initial-result])
      (define ch : (U EOF Char) (peek-char /dev/stdin skip))
      (cond [(or (eof-object? ch) (not (char-digit? ch)))
             (drop-bytes /dev/stdin (+ skip (if (and eat-last-whitespace? (char? ch) (char-whitespace? ch)) 1 0)))
             result]
            [else (read-octal (fx+ skip 1) (bitwise-ior (arithmetic-shift result bitwidth) (char->decimal ch)))]))))

(define read-limited-decimal : (-> Input-Port Byte Byte (-> Char Boolean) (-> Char Index) Natural Nonnegative-Fixnum Boolean (Values Nonnegative-Fixnum Byte))
  (lambda [/dev/stdin max-ndigit bitwidth char-digit? char->decimal skip initial-result eat-last-whitespace?]
    (when (> skip 0)
      (drop-bytes /dev/stdin skip))
 
    (let read-octadecimal ([rest : Byte max-ndigit]
                           [skip : Natural 0]
                           [result : Nonnegative-Fixnum initial-result])
      (define ch : (U EOF Char) (peek-char /dev/stdin skip))
      (cond [(or (eof-object? ch) (not (char-digit? ch)) (zero? rest))
             (drop-bytes /dev/stdin (+ skip (if (and eat-last-whitespace? (char? ch) (char-whitespace? ch)) 1 0)))
             (values result rest)]
            [else (read-octadecimal (fx- rest 1) (fx+ skip 1)
                                    (fxior (fxlshift result bitwidth)
                                           (char->decimal ch)))]))))

(define peek-flexible-decimal : (-> Input-Port Natural Byte Nonnegative-Fixnum (-> Char Boolean) (-> Char Index) Nonnegative-Fixnum Nonnegative-Fixnum
                                    (Values Nonnegative-Fixnum Byte))
  (lambda [/dev/stdin skip bitwidth ceiling char-digit? char->decimal initial-result initial-count]
    (let peek-decimal ([skip : Natural skip]
                       [result : Nonnegative-Fixnum initial-result]
                       [count : Nonnegative-Fixnum initial-count])
      (define hex : (U EOF Char) (peek-char /dev/stdin skip))
    
      (cond [(or (eof-object? hex) (not (char-digit? hex))) (values result (assert count byte?))]
            [else (let ([result++ (fxior (fxlshift result bitwidth) (char->decimal hex))])
                    (cond [(<= result++ ceiling) (peek-decimal (fx+ skip 1) result++ (fx+ count 1))]
                          [else (values result (assert count byte?))]))]))))
