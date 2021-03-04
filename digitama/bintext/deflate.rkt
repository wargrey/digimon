#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.2.0.txt
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
    (define literal-frequencies : (Vectorof Index) (smart-make-vector uplitcodes (or no-compression? (not allow-dynamic?)) lz77-dictionary-placeholder 0))
    (define distance-frequencies : (Vectorof Index) (smart-make-vector updistcodes (or no-compression? (not allow-dynamic?)) lz77-dictionary-placeholder 0))

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
        (PUSH-BITS EOB EOBbits)
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
                            [extra (unsafe-bytes-ref huffman-backref-extra-bits s-idx)])
                       (PUSH-BITS (unsafe-vector*-ref literals hspan) (unsafe-vector*-ref huffman-fixed-literal-lengths hspan))
                       (when (> extra 0) (PUSH-BITS (unsafe-idx- misc (unsafe-vector*-ref huffman-backref-bases s-idx)) extra)))
                     
                     (let* ([extra (unsafe-bytes-ref huffman-distance-extra-bits hdist)])
                       (PUSH-BITS (unsafe-vector*-ref distances hdist) (unsafe-vector*-ref huffman-fixed-distance-lengths hdist))
                       (when (> extra 0) (PUSH-BITS (unsafe-idx- dist (unsafe-vector*-ref huffman-distance-bases hdist)) extra))))])
            
            (write-codeword (+ idx 1))))
        
        (PUSH-BITS EOB EOBbits)
        (SEND-BITS #:windup? BFINAL)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define huffman-calculate-stored-block-length : (-> Bytes Index Index Nonnegative-Fixnum)
      (lambda [bsrc start end]       
        (+ 8 32 (unsafe-idxlshift (unsafe-idx- end start) 3))))

    (define huffman-calculate-static-block-length : (-> (Vectorof Index) (Vectorof Index) Index Index Nonnegative-Fixnum)
      (lambda [l77src dists start end]
        (define codeword-size : Index
          (let fold-codeword ([idx : Nonnegative-Fixnum start]
                              [bitsize : Index 0])
             (cond [(>= idx end) bitsize]
                   [else (let ([misc (unsafe-vector*-ref l77src idx)]
                               [dist (unsafe-vector*-ref dists idx)])
                           (define code-length : Index
                             (if (= dist 0)
                                 ; pure literals
                                 (unsafe-vector*-ref huffman-fixed-literal-lengths misc)

                                 ; <span, backward distance>
                                 (unsafe-idx+
                                  (let* ([hspan (backref-span->huffman-symbol misc)]
                                         [s-idx (unsafe-idx- hspan backref-span-offset)])
                                    (+ (unsafe-vector*-ref huffman-fixed-literal-lengths hspan)
                                       (unsafe-bytes-ref huffman-backref-extra-bits s-idx)))
                                  
                                  (let ([hdist (backref-distance->huffman-distance dist)])
                                    (+ (unsafe-vector*-ref huffman-fixed-distance-lengths hdist)
                                       (unsafe-bytes-ref huffman-distance-extra-bits hdist))))))

                           (fold-codeword (+ idx 1) (unsafe-idx+ bitsize code-length)))])))
        
        (+ 3 EOBbits codeword-size)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (raw-block-flush [BFINAL : Boolean] [payload : Index] [flush-lz77? : Boolean]) : Void
      (cond [(not no-compression?)
             (when (> payload 0)
               ;; NOTE
               ; `lz77-deflate` will invoke `lz77-block-flush` whenever the lz77 block is full,
               ;   but which will never write the block with a FINAL flag depsite the value of `BFINAL`.
               ; Instead, another empty final block will be appended if there happens to be no more
               ;   content after flushing last full lz77 block. 

               ;; TODO: slide the window
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
      ; The handy thing is that backward distances are allowed to cross block boundaries,
      ;   thus, the only thing to do here is to figure out a good way encoding and
      ;   feed the bitstream resulting bits to output.
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
                                         (Boolean #:window-bits Positive-Byte #:name (Option String) #:error-name Symbol #:commit? Boolean #:permissive? Boolean)
                                         Input-Port)
  (lambda [#:window-bits [winbits window-ibits] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false] #:permissive? [permissive? #true]
           /dev/zipin csize [close-orig? #false]]
    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    
    (define literal-table : Huffman-Lookup-Table (force huffman-fixed-literal-lookup-table))
    (define distance-table : Huffman-Lookup-Table (force huffman-fixed-distance-lookup-table))
    (define codelen-lengths : Bytes (make-bytes upcodelens 0))

    (define window-size/2 : Index (unsafe-idxlshift 1 (max winbits 15))) ; should be at least 32KB
    (define window-size : Index (unsafe-idx+ window-size/2 window-size/2))
    (define window : Bytes (make-bytes window-size))

    ;; NOTE: The backward distances are allowed to cross block boundaries
    (define payload-idx : Index window-size/2)
    (define payload : Index 0)

    (define (huffman-try-slide-window [p-idx : Index] [current-payload : Index] [span : Index]) : (Values Index Index Nonnegative-Fixnum)
      (define slide-end : Index (unsafe-idx+ p-idx current-payload))
      (define payload++ : Nonnegative-Fixnum (+ current-payload span))
      
      (cond [(<= (+ slide-end span) window-size) (values p-idx slide-end payload++)]
            [else (let* ([slide-start (unsafe-idx- slide-end window-size/2)])
                    (unsafe-bytes-copy! window 0 window slide-start slide-end)
                    ; don't complicate it, the above line says "to translate it from `sliding-start` to 0",
                    ; thus, the subtrahend is just the `sliding-start` rather than the `window-size/2`.
                    (values (unsafe-idx- p-idx slide-start)
                            (unsafe-idx- slide-end slide-start)
                            payload++))]))

    (define (huffman-try-slide-window/stored-block [stored-idx : Index] [size : Index]) : (Values Index Index Index)
      (define stored-end : Nonnegative-Fixnum (unsafe-idx+ stored-idx size))
      
      (cond [(<= stored-end window-size) (values stored-idx stored-end size)]
            [(>= size window-size/2) (values 0 window-size/2 window-size/2)]
            [else (let* ([slide-start (unsafe-idx- stored-end window-size/2)])
                    (unsafe-bytes-copy! window 0 window slide-start stored-idx)
                    (values (unsafe-idx- stored-idx slide-start)
                            (unsafe-idx- stored-end slide-start)
                            size))]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-values (FEED-BITS PEEK-BITS FIRE-BITS $SHELL)
      (make-input-lsb-bitstream #:padding-byte #xFF #| <= see `read-huffman-symbol` |#
                                #:limited csize #| no need to make another input-port via `make-limited-input-port` |#
                                /dev/zipin))
    
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
      (set! payload (PEEK-BITS 16 #xFFFF))

      (let ([chksum (PEEK-BITS 16 #xFFFF 16)])
        (unless (= (unsafe-uint16-not payload) chksum)
          (throw-check-error /dev/blkin ename "stored[~a]: invalid block length: ~a ~a"
                             (if (not BFINAL?) #b0 #b1) (~binstring payload 16) (~binstring chksum 16))))

      (FIRE-BITS 32))
    
    (define (huffman-begin-static-block! [BFINAL? : Boolean]) : Any
      (unless (or (eq? BTYPE 'static) (not BTYPE))
        (set! literal-table (force huffman-fixed-literal-lookup-table))
        (set! distance-table (force huffman-fixed-distance-lookup-table))))

    (define (huffman-begin-dynamic-block! [BFINAL? : Boolean]) : Any
      (unless (FEED-BITS 14)
        (throw-eof-error /dev/blkin 'huffman-begin-dynamic-block!))
      
      (define nhlit : Nonnegative-Fixnum (+ (PEEK-BITS 5 #x1F) literal-nbase))
      (define nhdist : Index (+ (PEEK-BITS 5 #x1F 5) distance-nbase))
      (define nhclen : Index (+ (PEEK-BITS 4 #xF 10) codelen-nbase))

      (FIRE-BITS 14)

      (when (not permissive?)
        ; the last two codes of both literals and distances are not unsed, and shouldn't be read from the stream.
        ; but some buggy implementation may use them, hence the `permissive?` as a workaround.
        
        (when (> nhlit strict-uplitcodes)
          (throw-range-error /dev/blkin ename (cons literal-nbase strict-uplitcodes) nhlit
                             "dynamic[~a]: too many number of literal codes: ~a(+~a)"
                             (if (not BFINAL?) #b0 #b1) (- nhlit literal-nbase) literal-nbase))
        
        (when (> nhdist strict-updistcodes)
          (throw-range-error /dev/blkin ename (cons distance-nbase strict-updistcodes) nhdist
                             "dynamic[~a]: too many number of distance codes: ~a(+~a)"
                             (if (not BFINAL?) #b0 #b1) (- nhdist distance-nbase) distance-nbase)))

      ; also note that the bits of header are extremely compact,
      ; so that they will never overflow the permissive upper boundaries.
      ; thus, we don't have to check the validity of `nhclen` here.

      (when (< nhclen upcodelens) ; the rest are zeros
        (bytes-fill! codelen-lengths 0))

      (let huffman-read-codelen-lengths! ([idx : Nonnegative-Fixnum 0])
        (when (< idx nhclen)
          (unless (FEED-BITS 3)
            (throw-eof-error /dev/blkin 'huffman-read-codelen-lengths!))

          (unsafe-bytes-set! codelen-lengths
                             (unsafe-bytes-ref codelen-lengths-order idx)
                             (PEEK-BITS 3 #b111))

          (FIRE-BITS 3)
          (huffman-read-codelen-lengths! (+ idx 1))))

      (huffman-begin-static-block! BFINAL?))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (read-stored-block! [zipout : Bytes] [start : Index] [end : Index] [BFINAL? : Boolean] [type : Symbol]) : Index
      (define stop : Nonnegative-Fixnum (min (+ start payload) end))

      (let copy-bytes ([pos : Index start])
        (if (< pos stop)
            (let ([pos++ (unsafe-idx+ pos 1)])
              (unless (FEED-BITS 8)
                (throw-eof-error /dev/blkin 'read-stored-block!))

              (unsafe-bytes-set! zipout pos (PEEK-BITS 8 #xFF))
              (FIRE-BITS 8)
              (copy-bytes pos++))

            (let ([size (unsafe-idx- pos start)])
              (when (> size 0)
                (set! payload (unsafe-idx- payload size))

                ; recall that backref distances can cross block boundaries, the stored blocks are also counted on.
                (let-values ([(stored-start stored-end maybe-truncated-span) (huffman-try-slide-window/stored-block payload-idx size)])
                  (unsafe-bytes-copy! window stored-start zipout (- stop maybe-truncated-span) stop)
                  (set! payload-idx stored-end)))

              size))))

    (define (read-huffman-block! [zipout : Bytes] [start : Index] [end : Index] [BFINAL? : Boolean] [type : Symbol]) : Nonnegative-Fixnum
      (define request : Index (unsafe-idx- end start))
      (define-values (supply p-idx)
        (let lazy-extract : (Values Nonnegative-Fixnum Index)
          ([supply : Nonnegative-Fixnum payload]
           [widx : Index payload-idx])
          (cond [(>= supply request) (values supply widx)]
                [else (let ([misc (read-huffman-symbol literal-table huffman-fixed-literal-maxlength strict-uplitcodes BFINAL? type 'literal)])
                        (cond [(< misc EOB) ; pure literals
                               (let-values ([(widx++ ipos supply++) (huffman-try-slide-window widx supply 1)])
                                 (lz77-inflate-into window ipos misc)
                                 (lazy-extract supply++ widx++))]
                              [(> misc EOB) ; <span, backward distance>s, extra bits represent MSB machine (unsigned) integers
                               (let* ([span-idx (unsafe-idx- misc backref-span-offset)]
                                      [span (read-huffman-extra-datum span-idx huffman-backref-bases huffman-backref-extra-bits)]
                                      [hdist (read-huffman-symbol distance-table huffman-fixed-distance-maxlength strict-updistcodes BFINAL? type 'distance)]
                                      [distance (read-huffman-extra-datum hdist huffman-distance-bases huffman-distance-extra-bits)])
                                 (let-values ([(widx++ ipos supply++) (huffman-try-slide-window widx supply span)])
                                   (lz77-inflate-into window ipos distance span)
                                   (lazy-extract supply++ widx++)))]
                              [else #;EOB (values supply widx)]))])))

      (let ([consumed (min request supply)])
        ; `zero consumed` implies that no datum has been extracted this time as we employ a lazy strategy
        (when (> consumed 0)
          (let ([p-idx++ (unsafe-idx+ p-idx consumed)])
            (unsafe-bytes-copy! zipout start window p-idx p-idx++)
            (set! payload (unsafe-idx- supply consumed))
            (set! payload-idx p-idx++)))

        consumed))

    (define (read-huffman-symbol [table : Huffman-Lookup-Table] [maxlength : Byte] [upcodes : Index] [BFINAL? : Boolean] [btype : Any] [ctype : Any]) : Index
      (FEED-BITS upbits)

      (define-values (symbol-code code-length) (huffman-symbol-extract table (PEEK-BITS) maxlength))

      (when (= code-length 0)
        (let ([s (PEEK-BITS maxlength)])
          (throw-check-error /dev/blkin ename "~a[~a]: invalid ~a codeword: ~a (prefix free: ~a)"
                             btype (if (not BFINAL?) #b0 #b1) ctype
                             (~binstring s maxlength) (~binstring (bits-reverse-uint16 s maxlength) maxlength))))

      (when (>= symbol-code upcodes)
        (throw-range-error /dev/blkin ename (cons 0 (sub1 upcodes)) symbol-code
                           "~a[~a]: invalid ~a codeword: ~a"
                           btype (if (not BFINAL?) #b0 #b1) ctype
                           (~binstring symbol-code code-length)))

      (FIRE-BITS code-length)
      symbol-code)

    (define (read-huffman-extra-datum [idx : Index] [base-bits : (Immutable-Vectorof Index)] [extra-bits : Bytes]) : Index
      (define base : Index (unsafe-vector*-ref base-bits idx))
      (define extra : Byte (unsafe-bytes-ref extra-bits idx))

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
                 (unsafe-idx+ start
                              (case type
                                [(stored) (read-stored-block! zipout start end last? type)]
                                [(static dynamic) (read-huffman-block! zipout start end last? type)]
                                [else #;EOB 0])))
               
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
