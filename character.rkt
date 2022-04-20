#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-hexdigit? : (-> Char Boolean)
  (lambda [ch]
    (or (char-numeric? ch)
        (char-ci<=? #\a ch #\f))))

(define char->hexadecimal : (-> Char Index)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (unsafe-idx- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (unsafe-idx- (char->integer hexch) #x37)]
          [else (unsafe-idx- (char->integer hexch) #x30)])))
