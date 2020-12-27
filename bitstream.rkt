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
(define-type BitStream-Output-Shell (U 'start-over 'align 'drop 'save 'restore))

(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-input-lsb-bitstream : (->* ((U Input-Port (-> Input-Port))) ((U Integer Bytes) Byte #:limited Natural #:padding-byte Byte)
                                        (Values (->* (Byte) ((U Input-Port (-> Input-Port))) Boolean)
                                                (case-> [Byte Byte Byte -> Byte]
                                                        [Byte Index Byte -> Index]
                                                        [Byte Byte -> Byte]
                                                        [Byte Index -> Index]
                                                        [Byte -> Index])
                                                (-> Byte Void)
                                                (->* (BitStream-Input-Shell) ((U Input-Port (-> Input-Port))) (U Natural EOF Void))))
  (lambda [/dev/defin [mgz/size 0] [lookahead 8] #:limited [truncated 0] #:padding-byte [eof-byte #xFF]]
    (define mgz-capacity : Index
      (let ([c0 (if (bytes? mgz/size) (bytes-length mgz/size) mgz/size)])
        (cond [(not (and (index? c0) (> c0 0))) 4096]
              [(> c0 lookahead) c0]
              [else (+ lookahead 1)])))
    
    (define magazine : Bytes
      (cond [(and (bytes? mgz/size) (>= (bytes-length mgz/size) mgz-capacity)) mgz/size]
            [else (make-bytes mgz-capacity)]))
    
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
              (let ([read-size (max 0 (- mgz-payload lookahead))])
                (when (> truncated 0)
                  (set! stock (max 0 (- stock read-size))))
              
                ;; commit consumed bytes, except for the last `lookahead` ones,
                ;; which might be unwound(unpeeked) after you are done dealing with the bitstream.
                (read-bytes! magazine /dev/bsin 0 read-size)
                ;; awkwardly, the `unwind` operation has not been used or (therefore even) defined in this implementation.
                ;; if no `lookahead`, no bytes could be unwound even though they are fed but before fired.
                (set! mgz-start (min lookahead mgz-payload))))

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

(define open-output-lsb-bitstream : (->* (Output-Port) ((U Integer Bytes) Byte #:name Any #:restore? Boolean)
                                         (Values (case-> [Bytes Index Index Boolean -> Void]
                                                         [Integer Byte Index -> Void]
                                                         [Integer Byte -> Void])
                                                 (-> [#:windup? Boolean] [#:save? Boolean] Natural)
                                                 (-> BitStream-Output-Shell Void)))
  (lambda [/dev/bsout [tank/size 0] [calibre-in-byte 4] #:name [name /dev/bsout] #:restore? [restore? #false]]
    (define tank-capacity : Index
      (let ([c0 (if (bytes? tank/size) (bytes-length tank/size) tank/size)])
        (cond [(not (and (index? c0) (> c0 0))) 4096]
              [else (max c0 calibre-in-byte)])))
      
    (define tank : Bytes
      (cond [(and (bytes? tank/size) (>= (bytes-length tank/size) tank-capacity)) tank/size]
            [else (make-bytes tank-capacity)]))

    (define tank-payload : Index 0)
    (define payload : Natural 0) ; bit buffer
    (define pwidth : Index 0)    ; bits in bit buffer
    (define pre-sent : Natural 0)
    (define calibre : Index (* calibre-in-byte 8))
    
    (define inject-maxidx : Index
      (assert (max (- tank-capacity calibre-in-byte) 0) index?))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define push-bits : (case-> [Bytes Index Index Boolean -> Void]
                                [Integer Byte Index -> Void]
                                [Integer Byte -> Void])
      (case-lambda
        [(n nbits) (push-bits n nbits (unsafe-idx- (unsafe-idxlshift 1 nbits) 1))]
        [(n nbits fast-mask)
         (inject-bits (+ payload (arithmetic-shift (unsafe-fxand n fast-mask) pwidth))
                      (unsafe-idx+ pwidth nbits)
                      tank-payload)]
        [(bs start end align?)
         (let ([r (remainder pwidth 8)])
           (cond [(and (> r 0) (not align?)) (for ([b (in-bytes bs start end)]) (push-bits b 8 #xFF))]
                 [else (let align ([start++ : Nonnegative-Fixnum start]
                                   [r : Byte r])
                         (cond [(> r 0) (push-bits #b0 (unsafe-idx- 8 r) #b0) (align start++ 0)]
                               [(= pwidth 0) (inject-bytes bs start++ end tank-payload)]
                               [(< start++ end) (push-bits (unsafe-bytes-ref bs start++) 8 #xFF) (align (add1 start++) r)]))]))]))

    (define send-bits : (-> [#:windup? Boolean] [#:save? Boolean] Natural)
      (lambda [#:windup? [windup? #false] #:save? [save? #false]]
        (define count : Natural
          (cond [(<= tank-payload 0) pre-sent]
                 [else (begin0 (+ pre-sent (flush-bits tank-payload))
                               (set! tank-payload 0))]))

        (when (> pre-sent 0)
          (set! pre-sent 0))

        (begin0
          (cond [(not windup?) count]
                [else (let windup : Natural ([pwidth-- : Index pwidth]
                                             [payload-- : Natural payload]
                                             [count++ : Natural count])
                        (cond [(>= pwidth-- 8)
                               (write-byte (bitwise-and payload-- #xFF) /dev/bsout)
                               (windup (unsafe-idx- pwidth-- 8) (arithmetic-shift payload-- -8) (add1 count++))]
                              [(> pwidth-- 0)
                               (write-byte payload-- /dev/bsout)
                               (windup 0 0 (add1 count++))]
                              [else (set!-values (payload pwidth) (values 0 0)) count++]))])

          (unless (not save?)
            (bs-shell 'save)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Output-Shell Void)
      #:with /dev/bsout name
      #:ingredients [tank tank-payload payload pwidth]
      #:operation [push-bits send-bits])

    (define (flush-bits [payload : Index]) : Index
      (write-bytes tank /dev/bsout 0 payload))

    (define (inject-bits [payload++ : Natural] [pwidth++ : Index] [idx : Index]) : Void
      (cond [(< pwidth++ calibre)
             (set!-values (payload pwidth tank-payload) (values payload++ pwidth++ idx))]
            [(<= idx inject-maxidx)
             (case calibre-in-byte
               [(#x8) (integer->integer-bytes (bitwise-and payload++ #xFFFFFFFFFFFFFFFF) 8 #false #false tank idx)
                      (inject-bits (arithmetic-shift payload++ -64) (unsafe-idx- pwidth++ 64) (unsafe-idx+ idx 8))]
               [(#x4) (integer->integer-bytes (bitwise-and payload++ #xFFFFFFFF) 4 #false #false tank idx)
                      (inject-bits (arithmetic-shift payload++ -32) (unsafe-idx- pwidth++ 32) (unsafe-idx+ idx 4))]
               [(#x2) (unsafe-bytes-set! tank idx (bitwise-and payload++ #xFF))
                      (unsafe-bytes-set! tank (+ idx 1) (bitwise-and (arithmetic-shift payload++ -8) #xFF))
                      (inject-bits (arithmetic-shift payload++ -16) (unsafe-idx- pwidth++ 16) (unsafe-idx+ idx 2))]
               [else (unsafe-bytes-set! tank idx (bitwise-and payload++ #xFF))
                     (inject-bits (arithmetic-shift payload++ -8) (unsafe-idx- pwidth++ 8) (unsafe-idx+ idx 1))])]
            [else (set! pre-sent (+ pre-sent (flush-bits idx))) (inject-bits payload++ pwidth++ 0)]))

    (define (inject-bytes [bs : Bytes] [start : Natural] [end : Natural] [idx : Index]) : Void
      (define size : Integer (- end start))
      (define slots : Index (unsafe-idx- tank-capacity idx))

      (cond [(<= size 0) (set! tank-payload idx)]
            [(<= size slots) (unsafe-bytes-copy! tank idx bs start end) (inject-bytes bs end end (unsafe-idx+ idx size))]
            [else (let ([start++ (unsafe-idx+ start slots)])
                    (unsafe-bytes-copy! tank idx bs start start++)
                    (set! pre-sent (+ pre-sent (flush-bits tank-capacity)))
                    (inject-bytes bs start++ end 0))]))

    (unless (not restore?)
      (bs-shell 'restore))

    (values push-bits send-bits bs-shell)))
