#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-integer? : (-> Integer Boolean)
  (lambda [i]
    (or (<= 0 i #xD7FF)
        (<= #xE000 i #x10FFFF))))

(define char-octdigit? : (-> Char Boolean)
  (lambda [ch]
    (char-ci<=? #\0 ch #\7)))

(define char-hexdigit? : (-> Char Boolean)
  (lambda [ch]
    (or (char-numeric? ch)
        (char-ci<=? #\a ch #\f))))

(define char->octadecimal : (-> Char Index)
  (lambda [octch]
    (unsafe-idx- (char->integer octch) #x30)))

(define char->hexadecimal : (-> Char Index)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (unsafe-idx- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (unsafe-idx- (char->integer hexch) #x37)]
          [else (unsafe-idx- (char->integer hexch) #x30)])))
