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

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-output-deflated-block : (->* (Output-Port ZIP-Strategy)
                                          (Boolean #:window-bits Positive-Byte #:memory-level Positive-Byte
                                                   #:allow-dynamic-block? Boolean #:safe-flush-on-close? Boolean #:name Any)
                                          Output-Port)
  (lambda [#:window-bits [winbits window-obits] #:memory-level [memlevel 8]
           #:allow-dynamic-block? [allow-dynamic? #false] #:safe-flush-on-close? [safe-flush-on-close? #true] #:name [name '/dev/dfbout]
           /dev/zipout strategy [close-orig? #false]]
    (define no-compression? : Boolean (= (zip-strategy-level strategy) 0))
    (define memory-level : Positive-Byte (min memlevel 9))
    
    ;;; NOTE
    ; The `block splitting` is a tough optimization problem.
    ; For the sake of simplicity, we set three kinds of block
    ; which are all controlled by `memory-level` and applied to all strategies
    ;   raw block:  receives the written bytes from clients;
    ;   lz77 block: receives the lz77 symbols transformed from the `raw block`,
    ;                 slots of `lz77-dists` are always paired with the corresponding slots of the lz77 block;
    ;   bits block: receives the final codes encoded from the `lz77 block`,
    ;                 residing in the LSB bitstream.

    (define raw-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 5)))
    (define lz77-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 6)))
    (define bits-blocksize : Positive-Index raw-blocksize)
    
    (define raw-block : Bytes (make-bytes raw-blocksize))
    (define lz77-block : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression? lz77-dictionary-placeholder 0))
    (define lz77-dists : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression? lz77-dictionary-placeholder 0))
    (define bitank : Bytes (make-bytes bits-blocksize))

    (define raw-payload : Index 0)
    (define lz77-payload : Index 0)
    
    ;; for LZ77
    (define hash-bits : Positive-Index (+ memory-level 7))
    (define hash-size : Positive-Index (unsafe-idxlshift 1 (min hash-bits lz77-default-safe-hash-bits)))
    (define window-size : Index (unsafe-idxlshift 1 winbits))

    (define hash-heads : (Vectorof Index) (smart-make-vector hash-size no-compression? lz77-dictionary-placeholder 0))
    (define hash-prevs : (Vectorof Index) (smart-make-vector raw-blocksize no-compression? lz77-dictionary-placeholder 0))

    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx) ; <=> (submit-huffman-symbol sym 0 d-idx)
         (unsafe-vector*-set! lz77-block lz77-payload sym)
         (set! lz77-payload (unsafe-idx+ lz77-payload 1))
         (when (= lz77-payload lz77-blocksize)
           (void (lz77-block-flush #false lz77-payload)))]
        [(distance span d-idx)
         (unsafe-vector*-set! lz77-block lz77-payload span)
         (unsafe-vector*-set! lz77-dists lz77-payload distance)
         (set! lz77-payload (unsafe-idx+ lz77-payload 1))
         (when (= lz77-payload lz77-blocksize)
           (void (lz77-block-flush #false lz77-payload)))]))

    ;; for (dynamic) huffman tree
    (define literal-frequencies : (Vectorof Index) (smart-make-vector upcodes (or no-compression? (not allow-dynamic?)) lz77-dictionary-placeholder 0))
    (define distance-frequencies : (Vectorof Index) (smart-make-vector updistances (or no-compression? (not allow-dynamic?)) lz77-dictionary-placeholder 0))

    (define submit-huffman-symbol+frequency : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx) ; <=> (submit-huffman-symbol sym 0 d-idx)
         (unsafe-vector*-set! literal-frequencies sym (unsafe-idx+ (unsafe-vector*-ref literal-frequencies sym) 1))
         (submit-huffman-symbol sym d-idx)]
        [(distance span d-idx)
         (let ([hspan (backref-span->huffman-symbol span)]
               [hdist (backref-distance->huffman-distance span)])
           (unsafe-vector*-set! literal-frequencies hspan (unsafe-idx+ (unsafe-vector*-ref literal-frequencies hspan) 1))
           (unsafe-vector*-set! distance-frequencies hdist (unsafe-idx+ (unsafe-vector*-ref distance-frequencies hdist) 1))
           (submit-huffman-symbol distance span d-idx))]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; WARNING: the port counts its position from 1, whereas `file-position` reports it from 0... 
    (define deflated-size : Positive-Integer 1)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (lz77-block-flush [BFINAL : Boolean] [payload : Index]) : Void
      (let ([size++ (huffman-write-static-block! /dev/zipout lz77-block lz77-dists BFINAL 0 payload #:with bitank)])
        (vector-fill! lz77-dists 0)
        (set! deflated-size (+ deflated-size size++))
        (set! lz77-payload 0)))
    
    (define (raw-block-flush [BFINAL : Boolean] [payload : Index] [add-empty-final-block? : Boolean] [flush-lz77? : Boolean]) : Void
      (define written-size : Natural
        (cond [(not no-compression?)
               (when (> payload 0)
                 ;; NOTE
                 ; `lz77-deflate` will invoke `lz77-block-flush` whenever the lz77 block is full,
                 ;   but it will never write the block with a FINAL flag depsite the value of `BFINAL`.
                 ; Instead, another empty final block will be appended if there happens to be no more
                 ;   content after flushing last full lz77 block. 

                 ;; TODO: sliding the window
                 (lz77-deflate raw-block #:hash-bits hash-bits #:hash-heads hash-heads #:hash-chain hash-prevs
                               (if (not allow-dynamic?) submit-huffman-symbol submit-huffman-symbol+frequency)
                               strategy 0 payload))

               (cond [(not (or BFINAL flush-lz77?)) 0]
                     [(> lz77-payload 0) (lz77-block-flush BFINAL lz77-payload) 0]
                     [(and BFINAL) (huffman-write-empty-block! /dev/zipout BFINAL #:with bitank)]
                     [else 0])]
              [(> payload 0) (huffman-write-stored-block! /dev/zipout raw-block BFINAL 0 payload #:with bitank)]
              [(and BFINAL add-empty-final-block?) (huffman-write-empty-block! /dev/zipout BFINAL #:with bitank)]
              [else 0]))

      (when (exact-positive-integer? written-size)
        (set! deflated-size (+ deflated-size written-size)))
      
      (when (> raw-payload 0)
        (set! raw-payload 0)))
    
    (define (block-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define src-size : Integer (- end start))
      
      (cond [(<= src-size 0)
             ;;; NOTE
             ; By design, a terminate block would be inserted when flushing the port manually,
             ; a.k.a calling `flush-port`, in which case `non-block/buffered?` is `#false`;
             
             ; But when performing a non-block writing with an empty bytes,
             ; say, via `write-bytes-avail*`, it would reach here too, and
             ; the `non-block/buffered?` is `#true`. So that, doing flush as
             ; usual but do not insert the block as the final one.
             (raw-block-flush (not non-block/buffered?) raw-payload #true #true)]

            [else ; repeatedly fill the raw block and flush it if full.
             (let deflate ([src-size : Natural src-size]
                           [start : Natural start]
                           [available : Index (unsafe-idx- raw-blocksize raw-payload)])
               (if (< src-size available)
                   (begin
                     (bytes-copy! raw-block raw-payload bs start end)
                     (set! raw-payload (unsafe-idx+ raw-payload src-size)))
                   (let ([end (+ start available)])
                     (bytes-copy! raw-block raw-payload bs start end)
                     (raw-block-flush #false raw-blocksize #false #false)
                     (when (> src-size available)
                       (deflate (unsafe-idx- src-size available) end raw-blocksize)))))])

      ; Conventionally, non-block writing implies flushing
      (unless (not non-block/buffered?)
        (raw-block-flush #false raw-payload #false #true))
      
      (- end start))

    (define (block-close) : Void
      (unless (not safe-flush-on-close?)
        ;;; NOTE
        ; When `safe-flush-on-close?` is `#false`, it means the lifecycle of the port is maintained automatically,
        ; say by a custodian, and client API will manually terminate the block chain in some ways.
        ; Or flushing will probably make blocks being written to wrong positions.
        (raw-block-flush #true raw-payload #true #true))
      
      (unless (not close-orig?)
        (close-output-port /dev/zipout)))
    
    (make-output-port name always-evt block-write block-close
                      #false port-always-write-evt #false
                      #false void
                      (λ [] deflated-size)
                      #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural)
                                         (Boolean #:window-bits Positive-Byte
                                                  #:name String #:error-name Symbol #:commit? Boolean)
                                         Input-Port)
  (lambda [#:window-bits [winbits window-ibits] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]
           /dev/zipin csize [close-orig? #false]]
    (define-values (FEED-BITS PEEK-BITS FIRE-BITS $SHELL) (make-input-lsb-bitstream /dev/zipin #:padding-byte #x00 #:limited csize))

    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    (define stock : Index 0)
    
    (define literal-table : Huffman-Lookup-Table (force huffman-fixed-literal-lookup-table))
    (define distance-table : Huffman-Lookup-Table (force huffman-fixed-distance-lookup-table))

    (define window-size : Index (unsafe-idxlshift 1 winbits))
    (define window : Bytes (make-bytes (+ window-size window-size)))
    (define window-idx : Index window-size)

    (define (huffman-slide-window [current-stock : Index]) : Void
      (displayln (cons (+ window-idx current-stock) (+ window-size window-size)))
      (when (>= window-idx window-size)
        (let ([window-idx-- (unsafe-idx- window-idx window-size)])
          ; sliding always occurs before the window is full, so it's safe to just add the `currnt-stock`
          (unsafe-bytes-copy! window 0 window window-idx-- (+ window-idx current-stock))
          (set! window-idx window-idx--))))
    
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
                      [else (throw-check-error /dev/blkin ename "unknown deflated block type: ~a" (~binstring BTYPE 2))])
                    BFINAL?))
          (values 'EOB #true)))

    (define (huffman-begin-stored-block! [BFINAL? : Boolean]) : Any
      ($SHELL 'align)
      (FEED-BITS 32)
      (set! stock (PEEK-BITS 16 #xFFFF))

      (let ([chksum (PEEK-BITS 16 #xFFFF 16)])
        (unless (= (unsafe-uint16-not stock) chksum)
          (throw-check-error /dev/blkin ename "stored[~a]: invalid block length: ~a ~a"
                             (if (not BFINAL?) #b0 #b1) (~binstring stock 16) (~binstring chksum 16))))

      (FIRE-BITS 32))
    
    (define (huffman-begin-static-block! [BFINAL? : Boolean]) : Any
      (set! window-idx window-size)
      (set! stock 0)
      
      (unless (or (eq? BTYPE 'static) (not BTYPE))
        (set! literal-table (force huffman-fixed-literal-lookup-table))
        (set! distance-table (force huffman-fixed-distance-lookup-table))))

    (define (huffman-begin-dynamic-block! [BFINAL? : Boolean]) : Any
      (huffman-begin-static-block! BFINAL?))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (read-stored-block! [zipout : Bytes] [start : Index] [end : Index] [BFINAL? : Boolean] [type : Symbol]) : Index
      (define stop : Nonnegative-Fixnum (min (+ start stock) end))
      
      (let copy-bytes ([pos : Index start])
        (cond [(= pos stop) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos]
              [(not (FEED-BITS 8)) (set! stock (unsafe-idx- stock (unsafe-idx- pos start))) pos] ; unexpected EOF
              [else (let ([start++ (unsafe-idx+ pos 1)])
                      (unsafe-bytes-set! zipout pos (PEEK-BITS 8 #xFF))
                      (FIRE-BITS 8)
                      (copy-bytes start++))])))

    (define (read-huffman-block! [zipout : Bytes] [start : Index] [end : Index] [BFINAL? : Boolean] [type : Symbol]) : Index
      (define request : Index (unsafe-idx- end start))
      (define supply : Index
        (let lazy-decode ([supply : Index stock])
          (cond [(>= supply request) supply]
                [else (let ([misc (read-huffman-symbol literal-table huffman-fixed-literal-maxlength upcodes BFINAL? type 'literal)])
                        (cond [(< misc EOB) ; pure literals
                               (when (= supply window-size) (huffman-slide-window supply))
                               (unsafe-lz77-inflate-into window (+ window-idx supply) misc)
                               (lazy-decode (unsafe-idx+ supply 1))]
                              [(= misc EOB) supply]
                              [else ; <span, backward distance>, extra bits represent MSB machine (unsigned) integers
                               (let* ([s-idx (unsafe-idx- misc backref-span-offset)]
                                      [s-base (unsafe-vector*-ref huffman-backref-bases s-idx)]
                                      [s-extra (unsafe-vector*-ref huffman-backref-extra-bits s-idx)]
                                      [span (if (> s-extra 0) (unsafe-idx+ s-base (PEEK-BITS s-extra)) s-base)])
                                 (when (> s-extra 0) (FIRE-BITS s-extra))
                                 (let* ([hdist (read-huffman-symbol distance-table huffman-fixed-distance-maxlength updistances BFINAL? type 'distance)]
                                        [d-base (unsafe-vector*-ref huffman-distance-bases hdist)]
                                        [d-extra (unsafe-vector*-ref huffman-distance-extra-bits hdist)]
                                        [distance (if (> d-extra 0) (unsafe-idx+ d-base (PEEK-BITS d-extra)) d-base)])
                                   (when (> d-extra 0) (FIRE-BITS d-extra))
                                   (when (> (+ supply span) window-size) (huffman-slide-window supply))
                                   (unsafe-lz77-inflate-into window (+ window-idx supply) distance span)
                                   (lazy-decode (unsafe-idx+ supply span))))]))])))

      (let ([consumed (min request supply)])
        (when (> consumed 0)
          (let ([window-idx++ (unsafe-idx+ window-idx consumed)])
            (unsafe-bytes-copy! zipout start window window-idx window-idx++)
            (set! stock (unsafe-idx- supply consumed))
            (set! window-idx window-idx++)))
        
        (unsafe-idx+ start consumed)))

    (define (read-huffman-symbol [table : Huffman-Lookup-Table] [maxlength : Byte] [upcodes : Index] [BFINAL? : Boolean] [btype : Any] [ctype : Any]) : Index
      (FEED-BITS upbits)

      (define-values (symbol-code code-length) (huffman-symbol-extract table (PEEK-BITS) maxlength))

      (when (= code-length 0)
        (throw-check-error /dev/blkin ename "~a[~a]: invalid ~a codeword: ~a"
                           btype (if (not BFINAL?) #b0 #b1) ctype
                           (~binstring (PEEK-BITS maxlength) maxlength)))

      (when (>= symbol-code upcodes)
        (throw-check-error /dev/blkin ename "~a[~a]: invalid ~a codeword: ~a(~a >= ~a)"
                           btype (if (not BFINAL?) #b0 #b1) ctype
                           (~binstring symbol-code code-length) symbol-code upcodes))

      (FIRE-BITS code-length)
      symbol-code)

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
                   [(stored) (read-stored-block! zipout start end last? type)]
                   [(static dynamic) (read-huffman-block! zipout start end last? type)]
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
    (PUSH-BITS EOB (unsafe-vector*-ref huffman-fixed-literal-lengths EOB))
    (SEND-BITS #:windup? BFINAL #:save? #true)))

(define huffman-write-stored-block! : (-> Output-Port Bytes Boolean Index Index [#:with (U Bytes Integer)] Natural)
  (lambda [/dev/zipout bsrc BFINAL start end #:with [tank 0]]
    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout tank 1 #:restore? #true))
    (define size : Index (unsafe-idx- end start))

    (PUSH-BITS (if BFINAL #b001 #b000) 3 #b111)
    ($SHELL 'align)
    (PUSH-BITS size 16 #xFFFF)
    (PUSH-BITS (unsafe-uint16-not size) 16 #xFFFF)
    (PUSH-BITS bsrc start end #true)
    
    (SEND-BITS #:windup? BFINAL #:save? #true)))

(define huffman-write-static-block! : (-> Output-Port (Vectorof Index) (Vectorof Index) Boolean Index Index [#:with (U Bytes Integer)] Natural)
  (lambda [/dev/zipout l77src dists BFINAL start end #:with [tank 0]]
    (define literals : (Vectorof Index) (force huffman-fixed-literal-codewords))
    (define distances : (Vectorof Index) (force huffman-fixed-distance-codewords))
    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout tank #:restore? #true))
    
    (PUSH-BITS (if BFINAL #b011 #b010) 3 #b111)

    (let write-codeword ([idx : Nonnegative-Fixnum start])
      (when (< idx end)
        (define misc : Index (unsafe-vector*-ref l77src idx))
        (define dist : Index (unsafe-vector*-ref dists idx))
        
        (cond [(= dist 0) ; pure literals
               (PUSH-BITS (unsafe-vector*-ref literals misc)
                          (unsafe-vector*-ref huffman-fixed-literal-lengths misc))]

              [else ; <span, backward distance>, extra bits represent MSB machine (unsigned) integers
               (let ([hspan (backref-span->huffman-symbol misc)]
                     [hdist (backref-distance->huffman-distance dist)])
                 (let* ([s-idx (unsafe-idx- hspan backref-span-offset)]
                        [extra (unsafe-vector*-ref huffman-backref-extra-bits s-idx)])
                   (PUSH-BITS (unsafe-vector*-ref literals hspan) (unsafe-vector*-ref huffman-fixed-literal-lengths hspan))
                   (when (> extra 0) (PUSH-BITS (unsafe-idx- misc (unsafe-vector*-ref huffman-backref-bases s-idx)) extra)))
                 
                 (let* ([extra (unsafe-vector*-ref huffman-distance-extra-bits hdist)])
                   (PUSH-BITS (unsafe-vector*-ref distances hdist) (unsafe-vector*-ref huffman-fixed-distance-lengths hdist))
                   (when (> extra 0) (PUSH-BITS (unsafe-idx- dist (unsafe-vector*-ref huffman-distance-bases hdist)) extra))))])

        (write-codeword (+ idx 1))))

    (PUSH-BITS EOB (unsafe-vector*-ref huffman-fixed-literal-lengths EOB))
    (SEND-BITS #:windup? BFINAL #:save? #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-block-placeholder : (Mutable-Vectorof LZ77-Symbol) (make-vector 0))
(define lz77-dictionary-placeholder : (Mutable-Vectorof Index) (make-vector 0))

(define smart-make-vector : (All (t) (-> Index Boolean (Mutable-Vectorof t) t (Mutable-Vectorof t)))
  (lambda [size unused? placeholder defval]
    (cond [(not unused?) (make-vector size defval)]
          [else placeholder])))
