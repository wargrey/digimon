#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitstream.rkt")
(require "digitama/unsafe/ops.rkt")

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
        (cond [(< pwidth nbits) (draw-bits nbits /dev/bsin)]
              [else #true])))

    (define peek-bits : (case-> [Byte Index Byte -> Index]
                                [Byte Byte Byte -> Byte]
                                [Byte Index -> Index]
                                [Byte Byte -> Byte]
                                [Byte -> Index])
      (case-lambda
        [(nbits fast-mask skip) (bitwise-and (unsafe-idxrshift payload skip) fast-mask)]
        [(nbits fast-mask) (bitwise-and payload fast-mask)]
        [(nbits) (bitwise-and payload (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))]))

    (define fire-bits : (-> Byte Void)
      (lambda [nbits]
        (set! payload (unsafe-idxrshift payload nbits))
        (set! pwidth (unsafe-idx- pwidth nbits))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Input-Shell (U Natural EOF Void))
      #:with /dev/defin
      #:ingredients [magazine mgz-payload mgz-start payload pwidth]
      #:operation [peek-bits feed-bits fire-bits])

    (define (draw-bits [nbits : Byte] [/dev/?in : (U Input-Port (-> Input-Port))]) : Boolean
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
                     (draw-bits nbits /dev/bsin)]
                    [(or (eq? ?n 0) (eof-object? ?n)) ; the bitstream is infinite by design
                     (unsafe-bytes-set! magazine mgz-start eof-byte)
                     (set! mgz-payload (unsafe-idx+ mgz-start 1))
                     (draw-bits nbits /dev/bsin)
                     #false]
                    [else #| skip special values |# (draw-bits nbits /dev/bsin)])))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (values feed-bits peek-bits fire-bits bs-shell)))

(require digimon/format)
(define make-output-lsb-bitstream : (->* ((U Output-Port (-> Output-Port))) (Integer Byte)
                                         (Values (case-> [Integer Byte Index -> Void]
                                                         [Integer Byte -> Void]
                                                         [Byte -> Void])
                                                 (->* () (#:windup? Boolean (U Output-Port (-> Output-Port))) Index)
                                                 (->* (BitStream-Output-Shell) ((U Output-Port (-> Output-Port))) Void)))
  (lambda [/dev/defout [tank-size 4096] [calibre-in-byte 4]]
    (define tank-capacity : Index (assert (max tank-size 1) index?))
    (define tank : Bytes (make-bytes tank-capacity))
    (define tank-payload : Index 0)
    
    (define payload : Natural 0) ; bit buffer
    (define pwidth : Index 0)    ; bits in bit buffer
    (define calibre : Index (* calibre-in-byte 8))
    
    (define inject-maxidx : Index
      (assert (max (- tank-capacity calibre-in-byte) 0) index?))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define push-bits : (case-> [Integer Byte Index -> Void]
                                [Integer Byte -> Void]
                                [Byte -> Void])
      (case-lambda
        [(bs) (push-bits bs 8 #xFF)]
        [(n nbits) (push-bits n nbits (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))]
        [(n nbits fast-mask)
         (inject-bits (+ payload (arithmetic-shift (unsafe-fxand n fast-mask) pwidth))
                      (unsafe-idx+ pwidth nbits)
                      tank-payload)]))
    
    (define send-bits : (->* () ((U Output-Port (-> Output-Port)) #:windup? Boolean) Index)
      (lambda [[/dev/?out /dev/defout] #:windup? [windup? #false]]
        (define /dev/bsout : Output-Port (if (output-port? /dev/?out) /dev/?out (/dev/?out)))

        (define count : Index
         (let flush ([count : Index 0])
           (inject-bits payload pwidth tank-payload)
           (cond [(<= tank-payload 0) count]
                 [else (let ([delta (write-bytes tank /dev/bsout 0 tank-payload)])
                         (set! tank-payload 0)
                         (flush (unsafe-idx+ count delta)))])))


        (cond [(not windup?) count]
              [else (let windup : Index ([pwidth-- : Index pwidth]
                                         [payload-- : Natural payload]
                                         [count++ : Index count])
                      (cond [(>= pwidth-- 8)
                             (write-byte (bitwise-and payload-- #xFF) /dev/bsout)
                             (windup (unsafe-idx- pwidth-- 8) (arithmetic-shift payload-- -8) (unsafe-idx+ count++ 1))]
                            [(> pwidth-- 0)
                             (write-byte payload-- /dev/bsout)
                             (windup 0 0 (unsafe-idx+ count++ 1))]
                            [else (set!-values (payload pwidth) (values 0 0)) count++]))])))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Output-Shell Void)
      #:with /dev/defout
      #:ingredients [payload pwidth]
      #:operation [push-bits flush-bits])

    (define (inject-bits [payload++ : Natural] [pwidth++ : Index] [idx : Index]) : Void
      (if (and (>= pwidth++ calibre) (< idx inject-maxidx))
          (case calibre-in-byte
            [(8) (integer->integer-bytes (bitwise-and payload++ #xFFFFFFFFFFFFFFFF) 8 #false #false tank idx)
                 (inject-bits (arithmetic-shift payload++ -64) (unsafe-idx- pwidth++ 64) (unsafe-idx+ idx 8))]
            [(4) (integer->integer-bytes (bitwise-and payload++ #xFFFFFFFF) 4 #false #false tank idx)
                 (inject-bits (arithmetic-shift payload++ -32) (unsafe-idx- pwidth++ 32) (unsafe-idx+ idx 4))]
            [(2) (unsafe-bytes-set! tank idx (bitwise-and payload++ #xFF))
                 (unsafe-bytes-set! tank (+ idx 1) (bitwise-and (arithmetic-shift payload++ -8) #xFF))
                 (inject-bits (arithmetic-shift payload++ -16) (unsafe-idx- pwidth++ 16) (unsafe-idx+ idx 2))]
            [else (unsafe-bytes-set! tank idx (bitwise-and payload++ #xFF))
                  (inject-bits (arithmetic-shift payload++ -8) (unsafe-idx- pwidth++ 8) (unsafe-idx+ idx 1))])
          (set!-values (payload pwidth tank-payload) (values payload++ pwidth++ idx))))

    (values push-bits send-bits bs-shell)))
