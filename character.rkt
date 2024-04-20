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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.unicode.org/reports/tr44/#General_Category_Values
(define char-letter? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(lu ll lt lm lo))
         #true)))

(define char-cased-letter? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(lu ll lt))
         #true)))

(define char-mark? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(mn mc me))
         #true)))

(define char-number? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(nd nl no))
         #true)))

(define char-punctuation? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(pc pd ps pe pi pf po))
         #true)))

(define char-symbol? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(sm sc sk so))
         #true)))

(define char-math? : (-> Char Boolean)
  (lambda [ch] ; symbol other
    (eq? (char-general-category ch) 'so)))

(define char-separator? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(zs zp zl))
         #true)))

(define char-control? : (-> Char Boolean)
  (lambda [ch]
    (and (memq (char-general-category ch) '(cc cf cs co cn))
         #true)))

(define char-asian? : (-> Char Boolean)
  (lambda [ch] ; letter other
    (eq? (char-general-category ch) 'lo)))

(define char-emoji? : (-> Char Boolean)
  (lambda [ch] ; symbol other
    (eq? (char-general-category ch) 'so)))
