#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitstream.rkt")
(require "digitama/unsafe/ops.rkt")
(require "digitama/bintext/table/bits.rkt")

;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-1
;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-2
;;; collection://file/gunzip

;;; 0. Naturally bits within a byte are indexed from right to left
;;; 1. The LSB-first bitstream corresponds the little-endian integer
;;; 2. The MSB-first bitstream corresponds the big-endian integer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type BitStream-Input-Shell (U 'start-over 'align 'aggregate 'final-commit))
(define-type BitStream-Output-Shell (U 'start-over 'align 'aggregate 'drop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-input-lsb-bitstream : (->* (Input-Port) ((U Integer Bytes) #:lookahead Byte #:limited Natural #:padding-byte Byte)
                                        (Values (-> Byte Boolean)
                                                (case-> [Byte Byte Byte -> Byte]
                                                        [Byte Index Byte -> Index]
                                                        [Byte Byte -> Byte]
                                                        [Byte Index -> Index]
                                                        [Byte -> Index]
                                                        [-> Index])
                                                (-> Byte Void)
                                                (-> BitStream-Input-Shell Natural)))
  (lambda [/dev/bsin [mgz/size 0] #:lookahead [lookahead 8] #:limited [truncated 0] #:padding-byte [eof-byte #xFF]]
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
    (define eof-width : Natural 0)
    (define aggregate : Natural 0)

    (define stock : Natural
      (cond [(> truncated 0) truncated]
            [else mgz-capacity #| in order to eliminate evitable checks on `truncated` |#]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define feed-bits : (-> Byte Boolean)
      (lambda [nbits]
        (cond [(< pwidth nbits) (draw-bits nbits)]
              [(= eof-width 0) #true]
              [(>= eof-width pwidth) #false]
              [else (<= (- pwidth eof-width) nbits)])))

    (define peek-bits : (case-> [Byte Index Byte -> Index]
                                [Byte Byte Byte -> Byte]
                                [Byte Index -> Index]
                                [Byte Byte -> Byte]
                                [Byte -> Index]
                                [-> Index])
      (case-lambda
        [(nbits fast-mask skip) (bitwise-and (unsafe-idxrshift payload skip) fast-mask)]
        [(nbits fast-mask) (bitwise-and payload fast-mask)]
        [(nbits) (bitwise-and payload (bits-mask nbits))]
        [() payload]))

    (define fire-bits : (-> Byte Void)
      (lambda [nbits]
        (set! payload (unsafe-idxrshift payload nbits))
        (set! pwidth (unsafe-idx- pwidth nbits))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Input-Shell Natural)
      #:with /dev/bsin lookahead
      #:ingredients [magazine mgz-payload mgz-start payload pwidth eof-width aggregate]
      #:operation [peek-bits feed-bits fire-bits])

    (define (try-commit-bits) : Void
      (when (> mgz-payload lookahead)
        (define commit-size : Index (unsafe-idx- mgz-payload lookahead))

        ;; commit consumed bytes, except for the last `lookahead` ones,
        ;; which might be unwound(unpeeked) after you are done dealing with the bitstream,
        ;; be careful, don't commit to much exceeding the limit as the payload might contain padded phantom bytes.
        (cond [(= truncated 0)
               (read-bytes! magazine /dev/bsin 0 commit-size)
               (set! aggregate (+ aggregate commit-size))]
              [(> stock commit-size)
               (read-bytes! magazine /dev/bsin 0 commit-size)
               (set! stock (unsafe-idx- stock commit-size))
               (set! aggregate (+ aggregate commit-size))]
              [else
               (read-bytes! magazine /dev/bsin 0 stock)
               (set! aggregate (+ aggregate stock))
               (set! stock 0)]))

      (unless (= mgz-payload 0)
        ;; at beginning, `mgz-start` points to 0; later on, it will never point to 0 unless `lookahead` is 0.
        ;; awkwardly, the `unwind` operation has neiher been used nor (therefore even) defined in this implementation.
        ;;   so that bytes before `mgz-start` are meaningless before the `final-commit`. 
        ;; next time drawing bits from port, at most `lookahead` bytes will be reloaded as they are just not committed.
        (set! mgz-start (min lookahead mgz-payload))))

    (define (draw-phantom-bytes) : Void
      ; do padding one by one, in case a gentle client doesn't exploiting to much.
      (unsafe-bytes-set! magazine mgz-start eof-byte)
      (set! mgz-payload (unsafe-idx+ mgz-start 1))
      (set! eof-width (+ eof-width 8)))

    (define (draw-bits [nbits : Byte]) : Boolean
      (cond [(< mgz-start mgz-payload)
             ; `integer-bytes->integer` is only faster for uint32 and uint64.
             (let ([v (unsafe-bytes-ref magazine mgz-start)])
               (set! payload (unsafe-idx+ payload (unsafe-idxlshift v pwidth))) ; <= LSB
               (set! pwidth (unsafe-idx+ pwidth 8))
               (set! mgz-start (unsafe-idx+ mgz-start 1))
               (feed-bits nbits))]
            [(> stock 0) ; the port should have some bytes to be read
             (try-commit-bits)
             (let ([?n (peek-bytes-avail! magazine mgz-start #f /dev/bsin mgz-start (min (+ mgz-start stock) mgz-capacity))])
               (cond [(exact-positive-integer? ?n)
                      (set! mgz-payload (unsafe-idx+ mgz-start ?n))
                      (draw-bits nbits)]
                     [(or (eq? ?n 0) (eof-object? ?n)) ; the port has been exhausted
                      (set! stock 0)
                      (draw-phantom-bytes)
                      (draw-bits nbits)]
                     [else #| skip special values |# (draw-bits nbits)]))]
            [else ; the bitstream is infinite by design
             (try-commit-bits)
             (draw-phantom-bytes)
             (draw-bits nbits)]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (values feed-bits peek-bits fire-bits bs-shell)))

(define open-output-lsb-bitstream : (->* (Output-Port) ((U Integer Bytes) Byte)
                                         (Values (case-> [Bytes Index Index Boolean -> Void]
                                                         [Integer Byte Index -> Void]
                                                         [Integer Byte -> Void])
                                                 (-> [#:windup? Boolean] Natural)
                                                 (-> BitStream-Output-Shell Natural)))
  (lambda [/dev/bsout [tank/size 0] [calibre-in-byte 4]]
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
    (define aggregate : Natural 0)
    (define pre-sent : Natural 0)
    (define calibre : Index (* calibre-in-byte 8))
    
    (define inject-maxidx : Natural (max (- tank-capacity calibre-in-byte) 0))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define push-bits : (case-> [Bytes Index Index Boolean -> Void]
                                [Integer Byte Index -> Void]
                                [Integer Byte -> Void])
      (case-lambda
        [(n nbits) (push-bits n nbits (bits-mask nbits))]
        [(n nbits fast-mask)
         (inject-bits (bitwise-ior payload (arithmetic-shift (unsafe-fxand n fast-mask) pwidth))
                      (unsafe-idx+ pwidth nbits)
                      tank-payload)]
        [(bs start end align?)
         (let ([r (remainder (ann pwidth Nonnegative-Fixnum) 8)])
           (cond [(and (> r 0) (not align?)) (for ([b (in-bytes bs start end)]) (push-bits b 8 #xFF))]
                 [else (let align ([start++ : Nonnegative-Fixnum start]
                                   [r : Byte r])
                         (cond [(> r 0) (push-bits #b0 (unsafe-idx- 8 r) #b0) (align start++ 0)]
                               [(= pwidth 0) (inject-bytes bs start++ end tank-payload)]
                               [(< start++ end) (push-bits (unsafe-bytes-ref bs start++) 8 #xFF) (align (add1 start++) r)]))]))]))

    (define send-bits : (-> [#:windup? Boolean] Natural)
      (lambda [#:windup? [windup? #false] #:save? [save? #false]]
        (define count : Natural
          (cond [(<= tank-payload 0) pre-sent]
                 [else (begin0 (+ pre-sent (flush-bits tank-payload))
                               (set! tank-payload 0))]))

        (define sent : Natural
          (cond [(not windup?) count]
                [else (let windup : Natural ([pwidth-- : Natural pwidth]
                                             [payload-- : Natural payload]
                                             [count++ : Natural count])
                        (cond [(>= pwidth-- 8)
                               (write-byte (bitwise-and payload-- #xFF) /dev/bsout)
                               (windup (unsafe-idx- pwidth-- 8) (arithmetic-shift payload-- -8) (add1 count++))]
                              [(> pwidth-- 0)
                               (write-byte payload-- /dev/bsout)
                               (windup 0 0 (add1 count++))]
                              [else (set!-values (payload pwidth) (values 0 0)) count++]))]))

        (when (> pre-sent 0) (set! pre-sent 0))
        (set! aggregate (+ aggregate sent))
        sent))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-bitstream-shell bs-shell (#:-> BitStream-Output-Shell Natural)
      #:with /dev/bsout
      #:ingredients [tank tank-payload payload pwidth aggregate]
      #:operation [push-bits send-bits])

    (define (flush-bits [payload : Natural]) : Index
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

    (values push-bits send-bits bs-shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bits-reverse-uint8 : (case-> [Byte -> Byte]
                                     [Byte Index -> Byte])
  (case-lambda
    [(b) (unsafe-vector*-ref bits-reversed-bytes b)]
    [(b nbits)
     (let ([r8 (bits-reverse-uint8 b)]
           [diff (- 8 nbits)])
       (cond [(<= diff 0) r8]
             [(< diff 8) (unsafe-idxrshift r8 diff)]
             [else r8]))]))

(define bits-reverse-uint16 : (case-> [Index -> Index]
                                      [Index Index -> Index])
  (case-lambda
    [(s)
     (let ([lo (bitwise-and s #xFF)]
           [hi (bitwise-and (arithmetic-shift s -8) #xFF)])
       (bitwise-ior (unsafe-idxlshift (bits-reverse-uint8 lo) 8)
                    (bits-reverse-uint8 hi)))]
    [(s nbits)
     (cond [(and (byte? s) (<= nbits 8)) (bits-reverse-uint8 s nbits)]
           [else (let ([r16 (bits-reverse-uint16 s)]
                       [diff (- 16 nbits)])
                   (cond [(<= diff 0) r16]
                         [(< diff 16) (unsafe-idxrshift r16 diff)]
                         [else r16]))])]))
