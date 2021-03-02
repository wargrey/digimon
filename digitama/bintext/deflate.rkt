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
                                          (Boolean #:window-bits Positive-Byte #:memory-level Positive-Byte #:allow-dynamic-block? Boolean #:name Any)
                                          Output-Port)
  (lambda [#:window-bits [winbits window-obits] #:memory-level [memlevel 8] #:allow-dynamic-block? [allow-dynamic? #true] #:name [name '/dev/dfbout]
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

    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout bits-blocksize 1))
    (define raw-block : Bytes (make-bytes raw-blocksize))
    (define lz77-block : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression? lz77-dictionary-placeholder 0))
    (define lz77-dists : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression? lz77-dictionary-placeholder 0))

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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define huffman-write-empty-block! : (-> Boolean Natural)
      (lambda [BFINAL]
        (PUSH-BITS (if BFINAL #b011 #b010) 3 #b111) ; empty static block
        (PUSH-BITS EOB (unsafe-vector*-ref huffman-fixed-literal-lengths EOB))
        (SEND-BITS #:windup? BFINAL)))
    
    (define huffman-write-stored-block! : (-> Bytes Boolean Index Index Natural)
      (lambda [bsrc BFINAL start end]
        (define size : Index (unsafe-idx- end start))
        
        (PUSH-BITS (if BFINAL #b001 #b000) 3 #b111)
        ($SHELL 'align)
        (PUSH-BITS size 16 #xFFFF)
        (PUSH-BITS (unsafe-uint16-not size) 16 #xFFFF)
        (PUSH-BITS bsrc start end #true)
        
        (SEND-BITS #:windup? BFINAL)))
    
    (define huffman-write-static-block! : (-> (Vectorof Index) (Vectorof Index) Boolean Index Index Natural)
      (lambda [l77src dists BFINAL start end]
        (define literals : (Vectorof Index) (force huffman-fixed-literal-codewords))
        (define distances : (Vectorof Index) (force huffman-fixed-distance-codewords))
        
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
        (SEND-BITS #:windup? BFINAL)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (raw-block-flush [BFINAL : Boolean] [payload : Index] [flush-lz77? : Boolean]) : Void
      (cond [(not no-compression?)
             (when (> payload 0)
               ;; NOTE
               ; `lz77-deflate` will invoke `lz77-block-flush` whenever the lz77 block is full,
               ;   but which will never write the block with a FINAL flag depsite the value of `BFINAL`.
               ; Instead, another empty final block will be appended if there happens to be no more
               ;   content after flushing last full lz77 block. 

               ;; TODO: sliding the window
               (vector-fill! hash-heads 0)
               (vector-fill! hash-prevs 0)
               (lz77-deflate raw-block #:hash-bits hash-bits #:hash-heads hash-heads #:hash-chain hash-prevs
                             (if (not allow-dynamic?) submit-huffman-symbol submit-huffman-symbol+frequency)
                             strategy 0 payload))
             
             (when (or BFINAL flush-lz77?)
               (cond [(> lz77-payload 0) (lz77-block-flush BFINAL lz77-payload)]
                     [(and BFINAL) (huffman-write-empty-block! BFINAL)]))]
            [(> payload 0) (huffman-write-stored-block! raw-block BFINAL 0 payload)]
            [(and BFINAL) (huffman-write-empty-block! BFINAL)])

      (when (> raw-payload 0)
        (set! raw-payload 0)))

    (define (lz77-block-flush [BFINAL : Boolean] [payload : Index]) : Void
      ;;; NOTE
      ; The handy thing is that backward distances can cross block boundaries,
      ;   thus, the only thing to do here is to figure out a good way encoding
      ;   and feed the bitstream resulting bits to output.
      ; The `raw-block-flush` has the responsibility to feed the `lz77-deflate`
      ;   reasonable data with a proper environment.
      
      (let ([size++ (huffman-write-static-block! lz77-block lz77-dists BFINAL 0 payload)])
        (vector-fill! lz77-dists 0)
        (set! lz77-payload 0)))
    
    (define (block-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define received : Integer (- end start))
      
      (cond [(> received 0)
             (let write-raw ([recv : Natural received]
                             [start : Natural start]
                             [available : Index (unsafe-idx- raw-blocksize raw-payload)])
              (if (>= recv available)
                  (let ([start++ (unsafe-idx+ start available)])
                    (unsafe-bytes-copy! raw-block raw-payload bs start start++)
                    (raw-block-flush #false raw-blocksize #false)
                    (when (> recv available)
                      (write-raw (unsafe-idx- recv available) start++ raw-blocksize)))
                  (begin
                    (unsafe-bytes-copy! raw-block raw-payload bs start end)
                    (set! raw-payload (unsafe-idx+ raw-payload recv)))))]

            [(or non-block/buffered? #| non-block writing implies flushing |#
                 (<= received 0) #| via `flush-port` or non-block writing with an empty bytes |#)
             (raw-block-flush #false raw-payload #true)])
      
      (- end start))

    (define (block-close) : Void
      (raw-block-flush #true raw-payload #true)
      
      (unless (not close-orig?)
        (close-output-port /dev/zipout)))
    
    (make-output-port name always-evt block-write block-close
                      #false port-always-write-evt #false
                      #false void
                      (λ [] (add1 ($SHELL 'aggregate)))
                      #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural)
                                         (Boolean #:window-bits Positive-Byte #:name (Option String) #:error-name Symbol #:commit? Boolean)
                                         Input-Port)
  (lambda [#:window-bits [winbits window-ibits] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]
           /dev/zipin csize [close-orig? #false]]
    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    
    (define literal-table : Huffman-Lookup-Table (force huffman-fixed-literal-lookup-table))
    (define distance-table : Huffman-Lookup-Table (force huffman-fixed-distance-lookup-table))

    (define window-size/2 : Index (unsafe-idxlshift 1 winbits))
    (define window-size : Nonnegative-Fixnum (+ window-size/2 window-size/2))
    (define window : Bytes (make-bytes window-size))

    ;; NOTE: The backward distances are allowed to cross the block boundaries
    (define window-idx : Index window-size/2)
    (define stock : Index 0)

    (define (huffman-slide-window [current-stock : Index]) : Void
      (when (>= window-idx window-size/2)
        (let ([window-idx-- (unsafe-idx- window-idx window-size/2)])
          ; sliding always occurs before the window is full, so it's safe to just add the `current-stock`
          (unsafe-bytes-copy! window 0 window window-idx-- (+ window-idx current-stock))
          (set! window-idx window-idx--))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-values (FEED-BITS PEEK-BITS FIRE-BITS $SHELL) (make-input-lsb-bitstream /dev/zipin #:padding-byte #xFF #:limited csize))
    
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
      (define supply : Nonnegative-Fixnum
        (let lazy-extract ([supply : Nonnegative-Fixnum stock])
          (cond [(>= supply request) supply]
                [else (let ([misc (read-huffman-symbol literal-table huffman-fixed-literal-maxlength upcodes BFINAL? type 'literal)])
                        (cond [(< misc EOB) ; pure literals
                               (let ([supply++ (+ supply 1)])
                                 (when (>= window-idx window-size) (huffman-slide-window supply))
                                 (lz77-inflate-into window (unsafe-idx+ window-idx supply) misc)
                                 (lazy-extract supply++))]
                              [(> misc EOB) ; <span, backward distance>s, extra bits represent MSB machine (unsigned) integers
                               (let* ([span-idx (unsafe-idx- misc backref-span-offset)]
                                      [span (read-huffman-extra-datum span-idx huffman-backref-bases huffman-backref-extra-bits)]
                                      [supply++ (+ supply span)])
                                 (when (>= (+ window-idx supply++) window-size) (huffman-slide-window supply))
                                 (let* ([hdist (read-huffman-symbol distance-table huffman-fixed-distance-maxlength updistances BFINAL? type 'distance)]
                                        [distance (read-huffman-extra-datum hdist huffman-distance-bases huffman-distance-extra-bits)])
                                   (lz77-inflate-into window (unsafe-idx+ window-idx supply) distance span)
                                   (lazy-extract supply++)))]
                              [else #;EOB supply]))])))

      (let ([consumed (min request supply)])
        (when (> consumed 0)
          (let ([window-idx++ (unsafe-idx+ window-idx consumed)])
            (unsafe-bytes-copy! zipout start window window-idx window-idx++)
            (set! window-idx window-idx++)))
        (set! stock (unsafe-idx- supply consumed))
        (unsafe-idx+ start consumed)))

    (define (read-huffman-symbol [table : Huffman-Lookup-Table] [maxlength : Byte] [upcodes : Index] [BFINAL? : Boolean] [btype : Any] [ctype : Any]) : Index
      (FEED-BITS upbits)

      (define-values (symbol-code code-length) (huffman-symbol-extract table (PEEK-BITS) maxlength))

      (when (= code-length 0)
        (let ([s (PEEK-BITS maxlength)])
          (throw-check-error /dev/blkin ename "~a[~a]: invalid ~a codeword: ~a (prefix free: ~a)"
                             btype (if (not BFINAL?) #b0 #b1) ctype
                             (~binstring s maxlength) (~binstring (bits-reverse-uint16 s maxlength) maxlength))))

      (when (>= symbol-code upcodes)
        (throw-check-error /dev/blkin ename "~a[~a]: invalid ~a codeword: ~a (~a >= ~a)"
                           btype (if (not BFINAL?) #b0 #b1) ctype
                           (~binstring symbol-code code-length) symbol-code upcodes))

      (FIRE-BITS code-length)
      symbol-code)

    (define (read-huffman-extra-datum [idx : Index] [base-bits : (Immutable-Vectorof Index)] [extra-bits : (Immutable-Vectorof Byte)]) : Index
      (define base : Index (unsafe-vector*-ref base-bits idx))
      (define extra : Byte (unsafe-vector*-ref extra-bits idx))

      (cond [(= extra 0) base]
            [else (begin0 (unsafe-idx+ base (PEEK-BITS extra))
                          (FIRE-BITS extra))]))

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
                       #false #false #false void
                       (λ [] (add1 ($SHELL 'aggregate)))
                       #false))

    /dev/blkin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-block-placeholder : (Mutable-Vectorof LZ77-Symbol) (make-vector 0))
(define lz77-dictionary-placeholder : (Mutable-Vectorof Index) (make-vector 0))

(define smart-make-vector : (All (t) (-> Index Boolean (Mutable-Vectorof t) t (Mutable-Vectorof t)))
  (lambda [size unused? placeholder defval]
    (cond [(not unused?) (make-vector size defval)]
          [else placeholder])))
