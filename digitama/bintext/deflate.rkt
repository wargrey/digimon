#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-2.0.txt
;;; https://www.rfc-editor.org/rfc/rfc1951.html
;;; illumos://gate/usr/src/contrib/zlib/deflate.c

(provide (all-defined-out))

(require "lz77.rkt")
(require "huffman.rkt")
(require "zipconfig.rkt")

(require "../ioexn.rkt")
(require "../evt.rkt")

(require "../../port.rkt")
(require "../../bitstream.rkt")
(require "../../format.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-output-deflated-block : (->* (Output-Port ZIP-Strategy)
                                          (Boolean #:window-bits Positive-Byte #:memory-level Positive-Byte
                                                   #:dynamic-block? Boolean #:safe-flush-on-close? Boolean #:name Any)
                                          Output-Port)
  (lambda [#:window-bits [winbits window-bits] #:memory-level [memlevel 8]
           #:dynamic-block? [dynamic? #true] #:safe-flush-on-close? [safe-flush-on-close? #true] #:name [name '/dev/dfbout]
           /dev/zipout strategy [close-orig? #false]]
    (define need-huffman? : Boolean (> (zip-strategy-level strategy) 0))
    (define memory-level : Positive-Byte (min memlevel 9))
    
    ;;; NOTE
    ; The `block splitting` is a tough optimization problem.
    ; For the sake of simplicity, we set three kinds of block
    ; which are all controlled by `memory-level` and applied to all strategies
    ;   raw block:  receives the written bytes from clients;
    ;   lz77 block: receives the lz77 symbols transformed from the `raw block`;
    ;   bits block: receives the final codes encoded from the `lz77 block`,
    ;                 residing in the LSB bitstream.

    (define raw-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 5)))
    (define lz77-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 6)))
    (define bits-blocksize : Positive-Index raw-blocksize)
    
    (define raw-block : Bytes (make-bytes raw-blocksize))
    (define lz77-block : (Vectorof LZ77-Symbol) (if (not need-huffman?) lz77-block-placeholder (make-vector lz77-blocksize 0)))
    (define bitank : Bytes (make-bytes bits-blocksize))

    (define raw-payload : Index 0)
    (define lz77-payload : Index 0)
    
    ; for LZ77
    (define hash-bits : Positive-Index (+ memory-level 7))
    (define hash-size : Positive-Index (unsafe-idxlshift 1 (min hash-bits lz77-default-safe-hash-bits)))
    (define window-size : Index (unsafe-idxlshift 1 winbits))

    (define-values (hash-heads hash-prevs code-freqs)
      (if (not need-huffman?)
          (values lz77-dictionary-placeholder lz77-dictionary-placeholder lz77-dictionary-placeholder)
          (values ((inst make-vector Index) hash-size) ((inst make-vector Index) raw-blocksize) ((inst make-vector Index) upcodewords 0))))

    (define huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx)
         (unsafe-vector*-set! code-freqs sym (unsafe-idx+ (unsafe-vector*-ref code-freqs sym) 1))
         (unsafe-vector*-set! lz77-block lz77-payload sym)
         (set! lz77-payload (unsafe-idx+ lz77-payload 1))]
        [(distance span d-idx)
         (let ([sym (backref-span->huffman-symbol span)])
           (unsafe-vector*-set! code-freqs sym (unsafe-idx+ (unsafe-vector*-ref code-freqs sym) 1))
           (unsafe-vector*-set! lz77-block lz77-payload (lz77-backref-pair distance span))
           (set! lz77-payload (unsafe-idx+ lz77-payload 1)))]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; WARNING
    ; the port counts its position starting from 1,
    ; whereas `file-position` reports it from 0... 
    (define deflated-size : Positive-Integer 1)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (block-flush [BFINAL : Boolean] [payload : Index] [add-empty-final-block? : Boolean]) : Void
      (define written-size : Natural
        (cond [(= payload 0) (or (and BFINAL add-empty-final-block? (huffman-write-empty-block! /dev/zipout BFINAL #:with bitank)) 0)]
              [(not need-huffman?) (huffman-write-stored-block! /dev/zipout raw-block BFINAL 0 payload #:with bitank)]
              [else (huffman-write-stored-block! /dev/zipout raw-block BFINAL 0 payload #:with bitank)]))

      (when (> written-size 0)
        (set! deflated-size (+ deflated-size written-size))
        (set! raw-payload 0)))
    
    (define (block-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define src-size : Integer (- end start))
      
      (if (<= src-size 0)
          ;;; NOTE
          ; By design, a terminate block would be inserted when flushing the port manually,
          ; a.k.a calling `flush-port`, in which case `non-block/buffered?` is `#false`;
          
          ; But when performing a non-block writing with an empty bytes,
          ; say, via `write-bytes-avail*`, it would reach here too, and
          ; the `non-block/buffered?` is `#true`. So that, doing flush as
          ; usual but do not insert the block as the final one.
          (block-flush (not non-block/buffered?) raw-payload #true)
          
          (let deflate ([src-size : Natural src-size]
                        [start : Natural start]
                        [available : Index (unsafe-idx- raw-blocksize raw-payload)])
            (if (< src-size available)
                (begin
                  (bytes-copy! raw-block raw-payload bs start end)
                  (set! raw-payload (unsafe-idx+ raw-payload src-size)))
                (let ([end (+ start available)])
                  (bytes-copy! raw-block raw-payload bs start end)
                  (block-flush #false raw-blocksize #false)
                  (when (> src-size available)
                    (deflate (unsafe-idx- src-size available) end raw-blocksize))))))

      ; Conventionally, non-block writing implies flush
      (unless (not non-block/buffered?)
        (block-flush #false raw-payload #false))
      
      (- end start))

    (define (block-close) : Void
      (unless (not safe-flush-on-close?)
        ;;; NOTE
        ; When `safe-flush-on-close?` is `#false`, it means the lifecycle of the port is maintained automatically,
        ; say by a custodian, and client API will manually terminate the block chain in some ways.
        ; Or flushing will probably make blocks being written to wrong positions.
        (block-flush #true raw-payload #true))
      
      (unless (not close-orig?)
        (close-output-port /dev/zipout)))
    
    (make-output-port name always-evt block-write block-close
                      #false port-always-write-evt #false
                      #false void
                      (λ [] deflated-size)
                      #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural) (Boolean #:name String #:error-name Symbol #:commit? Boolean) Input-Port)
  (lambda [/dev/zipin csize [close-orig? #false] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]]
    (define-values (FEED-BITS PEEK-BITS FIRE-BITS $SHELL) (make-input-lsb-bitstream /dev/zipin #:padding-byte #xFF #:limited csize))

    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    (define stock : Index 0)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (huffman-read-block-header!) : (Values Symbol Boolean)
      (if (FEED-BITS 3)
          (let ([BFINAL? (= (PEEK-BITS 1 #b1) 1)]
                [BTYPE (PEEK-BITS 2 #b11 1)])
            (FIRE-BITS 3)
            (values (case BTYPE
                      [(#b00) (huffman-begin-stored-block! BFINAL?) 'stored]
                      [(#b01) (huffman-begin-static-block! BFINAL?) 'static]
                      [(#b10) (huffman-begin-dynamic-block! BFINAL?) 'dynamic]
                      [else (throw-check-error /dev/blkin ename "unknown deflated block type")])
                    BFINAL?))
          (values 'EOB #true)))

    (define (huffman-begin-stored-block! [BFINAL? : Boolean]) : Any
      ($SHELL 'align)
      (FEED-BITS 32)
      (set! stock (PEEK-BITS 16 #xFFFF))

      (let ([chksum (PEEK-BITS 16 #xFFFF 16)])
        (unless (= (unsafe-uint16-not stock) chksum)
          (throw-check-error /dev/blkin ename "stored[~a]: invalid block length: ~a ~a"
                             (if (not BFINAL?) #b0 #b1) (~binstring stock) (~binstring chksum))))

      (FIRE-BITS 32))
    
    (define (huffman-begin-static-block! [BFINAL? : Boolean]) : Any
      (void))

    (define (huffman-begin-dynamic-block! [BFINAL? : Boolean]) : Any
      (void))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (read-stored-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (define stop : Nonnegative-Fixnum (min (+ start stock) end))
      
      (let copy-bytes ([pos : Index start])
        (cond [(= pos stop) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos]
              [(not (FEED-BITS 8)) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos] ; unexpected EOF
              [else (let ([start++ (unsafe-idx+ pos 1)])
                      (unsafe-bytes-set! zipout pos (PEEK-BITS 8 #xFF))
                      (FIRE-BITS 8)
                      (copy-bytes start++))])))

    (define (read-static-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    (define (read-dynamic-block! [zipout : Bytes] [start : Index] [end : Index]) : Index
      (read-stored-block! zipout start end))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (deflate-read! [zipout : Bytes]) : Port-Reader-Plain-Datum
      (define end : Index (bytes-length zipout))

      (let read-block ([start : Index 0])
        (cond [(not (eq? BTYPE 'EOB))
               (define-values (type last?)
                 (cond [(not BTYPE) (huffman-read-block-header!)]
                       [else (values BTYPE BFINAL)]))

               (define start++ : Index
                 (case type
                   [(stored)  (read-stored-block! zipout start end)]
                   [(static)  (read-static-block! zipout start end)]
                   [(dynamic) (read-dynamic-block! zipout start end)]
                   [else #;EOB (values start)]))
               
               (cond [(>= start++ end) (set!-values (BTYPE BFINAL) (values type last?)) end]
                     [else (set! BTYPE (and last? 'EOB)) (read-block start++)])]
              [(= start 0) (unless (not commit?) ($SHELL 'final-commit)) eof]
              [else start])))
    
    (define /dev/blkin : Input-Port
      (make-input-port (or name (object-name /dev/zipin))
                       
                       deflate-read! #false
                       
                       (λ [] (when close-orig? (close-input-port /dev/zipin)))
                       
                       #false #false
                       
                       (lambda () (port-next-location /dev/zipin))
                       (lambda () (port-count-lines! /dev/zipin))
                       
                       1 #| initial position |#))

    /dev/blkin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define huffman-write-empty-block! : (-> Output-Port Boolean [#:with (U Bytes Integer)] Natural)
  (lambda [/dev/zipout BFINAL #:with [tank 0]]
    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout tank #:restore? #true))
    (PUSH-BITS (if BFINAL #b011 #b010) 3 #b111) ; empty static block
    (PUSH-BITS EOB 9 #x1FF)
    (SEND-BITS #:windup? BFINAL #:save? #true)))

(define huffman-write-stored-block! : (-> Output-Port Bytes Boolean Index Index [#:with (U Bytes Integer)] Natural)
  (lambda [/dev/zipout bsrc BFINAL start end #:with [tank 0]]
    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout tank #:restore? #true))
    (define size : Index (unsafe-idx- end start))

    (PUSH-BITS (if BFINAL #b001 #b000) 3 #b111)
    ($SHELL 'align)
    (PUSH-BITS size 16 #xFFFF)
    (PUSH-BITS (unsafe-uint16-not size) 16 #xFFFF)
    (PUSH-BITS bsrc start end #true)
    
    (SEND-BITS #:windup? BFINAL #:save? #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-block-placeholder : (Mutable-Vectorof (U Index (Pairof Index Index))) (make-vector 0))
(define lz77-dictionary-placeholder : (Mutable-Vectorof Index) (make-vector 0))
