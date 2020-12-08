#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitstream.rkt")

;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-1
;;; collection: file/gunzip

;;; 0. Naturally bits within a byte are indexed from right to left
;;; 1. The LSB-first bitstream corresponds the little-endian integer
;;; 2. The MSB-first bitstream corresponds the big-endian integer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type BitStream-Shell (U 'start-over 'align 'final-commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-input-lsb-bitstream : (->* () (Integer Byte #:eof-byte Byte)
                                        (Values (case-> [Byte Byte -> Byte]
                                                        [Byte -> Index])
                                                (-> Byte Boolean)
                                                (-> Byte Void)
                                                (-> BitStream-Shell (U Natural EOF Void))))
  (lambda [[mgz-size 4096] [lookahead 8] #:eof-byte [eof-byte #xFF]]
    ;; the magazine should be able to hold at least 256 bits, which is the ceiling for both `feed`
    (define mgz-capacity : Positive-Integer (max mgz-size 32))
    (define magazine (make-bytes mgz-capacity))
    
    (define mgz-payload : Index 0) ; number of bytes in magazine
    (define mgz-start : Index 0)   ; index into magazine = number of used peeked bytes

    (define payload : Index 0)     ; bit buffer
    (define pwidth : Index 0)      ; bits in bit buffer

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (feed-bits [n : Byte]) : Boolean
      (cond [(< pwidth n) (load-bits n)]
            [else #true]))
    
    (define (load-bits [nbits : Byte]) : Boolean
      (if (< mgz-start mgz-payload)
          (let ([v (unsafe-bytes-ref magazine mgz-start)])
            (set! payload (unsafe-fx+ payload (unsafe-fxlshift v pwidth))) ; <= LSB
            (set! pwidth (unsafe-fx+ pwidth 8))
            (set! mgz-start (unsafe-fx+ mgz-start 1))
            (feed-bits nbits))
          
          (let ([/dev/stdin (current-input-port)])      
            (when (> mgz-payload 0)
              ;; commit consumed bytes, except for the last `lookahead` ones,
              ;; which might be unpeeked after you are done dealing with the bitstream.
              (read-bytes! magazine /dev/stdin 0 (max 0 (- mgz-payload lookahead)))
              ;; if no `lookahead`, no bytes could be unpeeked even though they are fed but not fired.
              (set! mgz-start (min lookahead mgz-payload)))

            (let ([?n (peek-bytes-avail! magazine mgz-start #f /dev/stdin mgz-start mgz-capacity)])
              (cond [(and (fixnum? ?n) (> ?n 0)) ; `peek-bytes-avail!` never returns 0 in this situation.
                     (set! mgz-payload (unsafe-fx+ mgz-start ?n))
                     (load-bits nbits)]
                    [(eof-object? ?n) ; the bitstream is infinite by design
                     (unsafe-bytes-set! magazine mgz-start eof-byte)
                     (set! mgz-payload (unsafe-fx+ mgz-start 1))
                     (load-bits nbits)
                     #false]
                    [else #| skip special values |# (load-bits nbits)])))))
    
    (define (fire-bits [nbits : Byte]) : Void
      (set! payload (unsafe-fxrshift payload nbits))
      (set! pwidth (unsafe-fx- pwidth nbits)))

    (define peek-bits : (case-> [Byte Byte -> Byte]
                                [Byte -> Index])
      (case-lambda
        [(nbits fast-mask) (unsafe-fxand payload fast-mask)]
        [(nbits) (unsafe-fxand payload (unsafe-fx- (unsafe-fxlshift 1 nbits) 1))]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Shell (U Natural EOF Void))
      #:ingredients [magazine mgz-payload mgz-start payload pwidth]
      #:operation [peek-bits feed-bits fire-bits])

    (values peek-bits feed-bits fire-bits bs-shell)))
