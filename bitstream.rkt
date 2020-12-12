#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitstream.rkt")
(require "digitama/unsafe/ops.rkt")
(require "number.rkt")

;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-1
;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-2
;;; collection://file/gunzip

;;; 0. Naturally bits within a byte are indexed from right to left
;;; 1. The LSB-first bitstream corresponds the little-endian integer
;;; 2. The MSB-first bitstream corresponds the big-endian integer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type BitStream-Input-Shell (U 'start-over 'align 'final-commit))
(define-type BitStream-Output-Shell (U 'start-over 'align 'drop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-input-lsb-bitstream : (->* ((U Input-Port (-> Input-Port))) (Integer Byte #:limited Natural #:padding-byte Byte)
                                        (Values (->* (Byte) ((U Input-Port (-> Input-Port))) Boolean)
                                                (case-> [Byte Byte Byte -> Byte]
                                                        [Byte Index Byte -> Index]
                                                        [Byte Byte -> Byte]
                                                        [Byte Index -> Index]
                                                        [Byte -> Index])
                                                (-> Byte Void)
                                                (->* (BitStream-Input-Shell) ((U Input-Port (-> Input-Port))) (U Natural EOF Void))))
  (lambda [/dev/defin [mgz-size 4096] [lookahead 8] #:limited [truncated 0] #:padding-byte [eof-byte #xFF]]
    (define mgz-capacity : Positive-Integer (if (> mgz-size lookahead) mgz-size (+ lookahead (max mgz-size 1))))
    (define magazine : Bytes (make-bytes mgz-capacity))
    
    (define mgz-payload : Index 0) ; number of bytes in magazine
    (define mgz-start : Index 0)   ; index into magazine = number of used peeked bytes
    (define payload : Index 0)     ; bit buffer
    (define pwidth : Index 0)      ; bits in bit buffer

    (define stock : Natural truncated)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define feed-bits : (->* (Byte) ((U Input-Port (-> Input-Port))) Boolean)
      (lambda [nbits [/dev/bsin /dev/defin]]
        (cond [(< pwidth nbits) (load-bits nbits /dev/bsin)]
              [else #true])))

    (define peek-bits : (case-> [Byte Index Byte -> Index]
                                [Byte Byte Byte -> Byte]
                                [Byte Index -> Index]
                                [Byte Byte -> Byte]
                                [Byte -> Index])
      (case-lambda
        [(nbits fast-mask skip) (unsafe-fxand (unsafe-idxrshift payload skip) fast-mask)]
        [(nbits fast-mask) (unsafe-fxand payload fast-mask)]
        [(nbits) (unsafe-fxand payload (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))]))

    (define fire-bits : (-> Byte Void)
      (lambda [nbits]
        (set! payload (unsafe-idxrshift payload nbits))
        (set! pwidth (unsafe-idx- pwidth nbits))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Input-Shell (U Natural EOF Void))
      #:with /dev/defin
      #:ingredients [magazine mgz-payload mgz-start payload pwidth]
      #:operation [peek-bits feed-bits fire-bits])

    (define (load-bits [nbits : Byte] [/dev/?in : (U Input-Port (-> Input-Port))]) : Boolean
      (if (< mgz-start mgz-payload)
          (let ([v (unsafe-bytes-ref magazine mgz-start)])
            (set! payload (unsafe-idx+ payload (unsafe-idxlshift v pwidth))) ; <= LSB
            (set! pwidth (unsafe-idx+ pwidth 8))
            (set! mgz-start (unsafe-idx+ mgz-start 1))
            (feed-bits nbits /dev/?in))
          
          (let ([/dev/bsin (if (input-port? /dev/?in) /dev/?in (/dev/?in))])      
            (when (> mgz-payload 0)
              (when (> truncated 0)
                (set! stock (max 0 (- stock mgz-payload))))
              
              ;; commit consumed bytes, except for the last `lookahead` ones,
              ;; which might be unwound(unpeeked) after you are done dealing with the bitstream.
              (read-bytes! magazine /dev/bsin 0 (max 0 (- mgz-payload lookahead)))
              ;; awkwardly, the `unwind` operation has not been used or (therefore even) defined in this implementation.
              ;; if no `lookahead`, no bytes could be unwound even though they are fed but before fired.
              (set! mgz-start (min lookahead mgz-payload)))

            (let* ([mgz-end (if (= truncated 0) mgz-capacity (min (+ mgz-start stock) mgz-capacity))]
                   [?n (peek-bytes-avail! magazine mgz-start #f /dev/bsin mgz-start mgz-end)])
              (cond [(and (fixnum? ?n) (> ?n 0))
                     (set! mgz-payload (unsafe-idx+ mgz-start ?n))
                     (load-bits nbits /dev/bsin)]
                    [(or (eq? ?n 0) (eof-object? ?n)) ; the bitstream is infinite by design
                     (unsafe-bytes-set! magazine mgz-start eof-byte)
                     (set! mgz-payload (unsafe-idx+ mgz-start 1))
                     (load-bits nbits /dev/bsin)
                     #false]
                    [else #| skip special values |# (load-bits nbits /dev/bsin)])))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (values feed-bits peek-bits fire-bits bs-shell)))


(define make-output-lsb-bitstream : (->* ((U Output-Port (-> Output-Port))) (Integer)
                                         (Values (case-> [Integer Byte Index -> Void]
                                                         [Integer Byte -> Void])
                                                 (->* () ((U Output-Port (-> Output-Port))) Index)
                                                 (->* (BitStream-Output-Shell) ((U Output-Port (-> Output-Port))) Void)))
  (lambda [/dev/defout [mgz-size 8]]
    (define mgz-capacity : Positive-Integer (max mgz-size 1))
    (define magazine (make-bytes mgz-capacity))
    
    (define payload : Index 0)     ; bit buffer
    (define pwidth : Index 0)      ; bits in bit buffer

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define push-bits : (case-> [Integer Byte Index -> Void]
                                [Integer Byte -> Void])
      (case-lambda
        [(n nbits fast-mask)
         (set! payload (unsafe-idx+ payload (unsafe-idxlshift (unsafe-fxand n fast-mask) pwidth)))
         (set! pwidth (unsafe-idx+ pwidth nbits))]
        [(n nbits)
         (push-bits n nbits (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))]))

    (define flush-bits : (->* () ((U Output-Port (-> Output-Port))) Index)
      (lambda [[/dev/?out /dev/defout]]
        (cond [(<= pwidth 0) 0]
              [else (let ([/dev/bsout (if (output-port? /dev/?out) /dev/?out (/dev/?out))])
                      (begin0 (cond [(<= pwidth 8) (write-byte payload /dev/bsout) 1]
                                    [else (let*-values ([(q r) (quotient/remainder pwidth 8)]
                                                        [(bsize) (if (> r 0) (unsafe-idx+ q 1) q)])
                                            (write-bytes (natural->memory-bytes payload bsize (and (<= bsize mgz-capacity) magazine) 0)
                                                         /dev/bsout 0 bsize))])

                              (set! payload 0)
                              (set! pwidth 0)))])))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Output-Shell Void)
      #:with /dev/defout
      #:ingredients [payload pwidth]
      #:operation [push-bits flush-bits])

    (values push-bits flush-bits bs-shell)))
