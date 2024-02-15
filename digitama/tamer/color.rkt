#lang racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/match)

(require file/convertible)

(require scribble/core)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg:rgb
  (lambda [c]
    (define hex (rgb-hex c))
    (hash-ref! fg-property-database hex
               (λ [] (color-property (color-hex->rgb-list hex))))))

(define fg-rgb
  (lambda [c [host #false]]
    (cond [(style? host)
           (make-style (style-name host)
                       (style-property-compose (style-properties host)
                                               (fg:rgb c)
                                               color-property?))]
          [(or (string? host) (symbol? host))
           (let ([rgb (rgb-list c)])
             (hash-ref! fg-rich-style-database (cons host rgb)
                        (λ [] (make-style host (list (fg:rgb rgb))))))]
          [else ; #false otherwise
           (let ([hex (rgb-hex c)])
             (hash-ref! fg-style-database hex
                        (λ [] (make-style #false (list (fg:rgb hex))))))])))

(define bg:rgb
  (lambda [c]
    (define hex (rgb-hex c))
    (hash-ref! bg-property-database hex
               (λ [] (background-color-property (color-hex->rgb-list hex))))))

(define bg-rgb
  (lambda [c [host #false]]
    (cond [(style? host)
           (make-style (style-name host)
                       (style-property-compose (style-properties host)
                                               (bg:rgb c)
                                               background-color-property?))]
          [(or (string? host) (symbol? host))
           (let ([rgb (rgb-list c)])
             (hash-ref! bg-rich-style-database (cons host rgb)
                        (λ [] (make-style host (list (bg:rgb rgb))))))]
          [else ; #false otherwise
           (let ([hex (rgb-hex c)])
             (hash-ref! bg-style-database hex
                        (λ [] (make-style #false (list (bg:rgb hex))))))])))

(define type-rgb
  (lambda [fgc bgc [host #false]]
    (bg-rgb bgc (fg-rgb fgc host))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fg-property-database (make-hasheq))
(define bg-property-database (make-hasheq))
(define fg-style-database (make-hasheq))
(define bg-style-database (make-hasheq))
(define fg-rich-style-database (make-hash))
(define bg-rich-style-database (make-hash))

(define rgb-hex
  (lambda [c]
    (define rgb (rgb-list c))
    (color-hex-from-rgb (car rgb) (cadr rgb) (caddr rgb))))

(define rgb-list
  (let ([fallback (list 0 0 0)])
    (lambda [c]
      (cond [(exact-nonnegative-integer? c) (color-hex->rgb-list c)]
            [(convertible? c) (convert c 'rgb-byte-list fallback)]
            [(color-property? c) (rgb-list (color-property-color c))]
            [(background-color-property? c) (rgb-list (background-color-property-color c))]
            [else (match c
                    [(list   (? byte? r) (? byte? g) (? byte? b) alpha ...) (list r g b)]
                    [(vector (? byte? r) (? byte? g) (? byte? b) alpha ...) (list r g b)]
                    [_ fallback])]))))

(define color-hex-from-rgb
  (lambda [r g b]
    (bitwise-ior (arithmetic-shift r 16)
                 (arithmetic-shift g 8)
                 b)))

(define color-hex->rgb-list
  (lambda [hex]
    (list (bitwise-and (arithmetic-shift hex -16) #xFF)
          (bitwise-and (arithmetic-shift hex -8) #xFF)
          (bitwise-and hex #xFF))))
