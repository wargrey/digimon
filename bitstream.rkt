#lang typed/racket/base

(provide (all-defined-out))

(require racket/unsafe/ops)

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/unsafe/ops
 [unsafe-fx+ (-> Index Nonnegative-Fixnum Index)]
 [unsafe-fx- (-> Index Index Index)]
 [unsafe-fxlshift (-> Byte Fixnum Index)]
 [unsafe-fxrshift (-> Index Byte Index)])

;;; https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-1
;;; collection: file/gunzip

;;; 0. Naturally bits within a byte are indexed from right to left; 
;;; 1. The LSB-first bitstream corresponds the little-endian integer;
;;; 2. The MSB-first bitstream corresponds the big-endian integer;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-lsb-bitstream-reader : (->* () (Integer Byte #:eof-byte Byte)
                                         (Values (case-> [Byte Byte -> Byte]
                                                         [Byte -> Index])
                                                 (-> Byte Boolean)
                                                 (-> Byte Void)
                                                 (-> (U Natural EOF))))
  (lambda [[mgz-size 4096] [lookahead 8] #:eof-byte [eof-byte #xFF]]
    ;; the magazine should be able to hold at least 256 bits, which is the ceiling for both `feed`
    (define mgz-capacity : Positive-Integer (max mgz-size 32))
    (define magazine (make-bytes mgz-capacity))
    
    (define mgz-payload : Index 0) ; number of bytes in magazine
    (define mgz-start : Index 0)   ; index into magazine = number of used peeked bytes

    (define payload : Index 0)     ; bit buffer
    (define pwidth : Index 0)      ; bits in bit buffer
  
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
              ;; commit consumed bytes, except for the last `lookahead` bytes, which we might unwind
              (read-bytes! magazine /dev/stdin 0 (max 0 (- mgz-payload lookahead)))
              ;; setting `mgz-start' to the number of unwound bytes lets us keep track of how much to not actually be `commit-bits`ed
              (set! mgz-start (min lookahead mgz-payload)))
            
            (let ([?n (peek-bytes-avail! magazine mgz-start #f /dev/stdin mgz-start mgz-capacity)])
              (cond [(and (fixnum? ?n) (> ?n 0)) ; `peek-bytes-avail!` will never return 0 in this situation.
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
        [(n fast-mask) (unsafe-fxand payload fast-mask)]
        [(nbits) (unsafe-fxand payload (unsafe-fx- (unsafe-fxlshift 1 nbits) 1))]))

    (define (commit-bits) : (U Natural EOF) ; do committing after finishing the work
      (let align-byte ()
        (when (>= pwidth 8)
          (set! pwidth (unsafe-fx- pwidth 8))
          (set! mgz-start (unsafe-fx- mgz-start 1))
          (align-byte)))
      (read-bytes! magazine (current-input-port) 0 mgz-start))

    (values peek-bits feed-bits fire-bits commit-bits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require digimon/format)

(define a #b1101)
(define b #b110)
(define c #b1011)
(define d #b00001)

(define abcd
  (+ a
     (arithmetic-shift b 4)
     (arithmetic-shift c 7)
     (arithmetic-shift d 11)))

(define bs (integer->integer-bytes abcd 2 #false #false))
(define /dev/stdin (open-input-bytes bs))

(define (deal-with-bits [v : Index]) : Void
  (displayln (~binstring v)))

(time (parameterize ([current-input-port /dev/stdin])
        (define-values (peek-bits feed-bits fire-bits commit-bits) (make-lsb-bitstream-reader))
        
        (bytes->bin-string bs #:separator " ")
        
        (let extract ()
          (when (feed-bits 16)
            (deal-with-bits (peek-bits 4)) (fire-bits 4)
            (deal-with-bits (peek-bits 3)) (fire-bits 3)
            (deal-with-bits (peek-bits 4)) (fire-bits 4)
            (deal-with-bits (peek-bits 5)) (fire-bits 5)
            (extract)))
        
        (commit-bits)))
