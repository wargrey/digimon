#lang typed/racket/base

(require digimon/bitstream)
(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/bsout (open-output-bytes))
(define-values (push-bits flush-bits _) (make-output-lsb-bitstream /dev/bsout))

(push-bits #b1101  4)
(push-bits #b110   3)
(push-bits #b1011  4)
(push-bits #b10001 5)
(flush-bits)

(define bs (get-output-bytes /dev/bsout #false))

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

(time (let-values ([(feed-bits peek-bits fire-bits bs-shell) (make-input-lsb-bitstream (open-input-bytes bs) 0 0)])
        (displayln (bytes->bin-string bs #:separator " "))
        
        (feed-bits 255)
        (fire-bits 3)
        (displayln (cons (~binstring (bs-shell 'align)) 'skipped))
        (deal-with-bits (peek-bits 8))
        (deal-with-bits (peek-bits 32))
        
        (bs-shell 'final-commit)))

(time (let-values ([(feed-bits peek-bits fire-bits bs-shell) (make-input-lsb-bitstream (open-input-bytes bs) 0 0 #:limited 1)])
        (displayln (cons 'limited (not (feed-bits 16))))
        
        (deal-with-bits (peek-bits 4 #b1111))
        (deal-with-bits (peek-bits 3 #b111   4))
        (deal-with-bits (peek-bits 4 #b1111  7))
        (deal-with-bits (peek-bits 5 #b11111 11))
        
        (bs-shell 'final-commit)))
