#lang typed/racket/base

(require digimon/bitstream)
(require digimon/number)
(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a #b1101)
(define b #b110)
(define c #b1011)
(define d #b10001)

(define abcd
  (+ a
     (arithmetic-shift b 4)
     (arithmetic-shift c 7)
     (arithmetic-shift d 11)))

(define bs (natural->memory-bytes abcd 2))

(define (deal-with-bits [v : Index]) : Void
  (displayln (~binstring v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let-values ([(feed-bits peek-bits fire-bits bs-shell) (make-input-lsb-bitstream (open-input-bytes (bytes-append (make-bytes 3000000) bs)))])
        (let extract ()
          (when (feed-bits 16)
            (peek-bits 4) (fire-bits 4)
            (peek-bits 3) (fire-bits 3)
            (peek-bits 4) (fire-bits 4)
            (peek-bits 5) (fire-bits 5)
            (extract)))
        
        (bs-shell 'final-commit)))

(time (let-values ([(feed-bits peek-bits fire-bits bs-shell) (make-input-lsb-bitstream (open-input-bytes bs))])
        (displayln (bytes->bin-string bs #:separator " "))
        
        (feed-bits 16)
        (fire-bits 3)
        (displayln (cons 'skipped (~binstring (bs-shell 'align))))
        (deal-with-bits (peek-bits 8))
        
        (bs-shell 'final-commit)))

(time (let-values ([(feed-bits peek-bits fire-bits bs-shell) (make-input-lsb-bitstream (open-input-bytes bs) #:limited 1)])
        (displayln (cons 'limited (not (feed-bits 16))))
        
        (deal-with-bits (peek-bits 4 #b1111))
        (deal-with-bits (peek-bits 3 #b111   4))
        (deal-with-bits (peek-bits 4 #b1111  7))
        (deal-with-bits (peek-bits 5 #b11111 11))
        
        (bs-shell 'final-commit)))
