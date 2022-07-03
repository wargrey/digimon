#lang typed/racket/base

;;; https://www.hanshq.net/zip.html
;;; https://www.euccas.me/zlib
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-output-deflated-block : (->* (Output-Port ZIP-Strategy)
                                          (Boolean #:window-bits Positive-Byte #:memory-level Positive-Byte #:fixed-only? Boolean
                                                   #:min-match Positive-Byte #:name Any)
                                          Output-Port)
  (lambda [#:window-bits [winbits window-obits] #:memory-level [memlevel 8] #:fixed-only? [static-only? #true]
           #:min-match [min-match lz77-default-min-match] #:name [name '/dev/dfbout]
           /dev/zipout strategy [close-orig? #false]]
    (define no-compression? : Boolean (= (zip-strategy-level strategy) 0))
    (define memory-level : Positive-Byte (min memlevel 9))
    
    ;;; NOTE
    ; The `block splitting` is a tough optimization problem.
    ; For the sake of simplicity, we set three blocks, which are all controlled by `memory-level` and applied to all strategies
    ;   raw block:  receives the data bytes from clients, when it is full, the lz77 deflating starts.
    ;                 By design, the sliding window consists of exact several raw blocks, which are filled and deflated consecutively,
    ;                 so that we can keep the history of the bytes that are incompressible and therefore have to be stored directly, 
    ;                 and save the cost of copying them into the sliding window. If the size of the sliding window is not the
    ;                 multiple of that of the raw block, don't forget to update the `raw-block-flush`.
    ;   lz77 block: receives the lz77 symbols transformed from the `raw block`, when it is full, the huffman encoding starts.
    ;                 Slots of `lz77-dists` are always paired with the corresponding slots of the lz77 block;
    ;   bits block: receives the huffman codewords encoded from the `lz77 block`, and in the charge of LSB bitstream facility.

    (define raw-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 5)))
    (define lz77-blocksize : Positive-Index (unsafe-idxlshift 1 (+ memory-level 6)))
    (define bits-blocksize : Positive-Index raw-blocksize)

    (define-values (PUSH-BITS SEND-BITS $SHELL) (open-output-lsb-bitstream /dev/zipout bits-blocksize 1))
    (define lz77-block : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression?))
    (define lz77-dists : (Vectorof Index) (smart-make-vector lz77-blocksize no-compression?))
    (define lz77-payload : Index 0)
    
    (define hash-bits : Positive-Index (+ memory-level 7))
    (define hash-size : Positive-Index (unsafe-idxlshift 1 (min hash-bits lz77-default-safe-hash-bits)))
    (define window-size/2 : Index (unsafe-idxlshift 1 winbits))
    (define window-size : Index (unsafe-idx* window-size/2 2))
    (define window : Bytes (make-bytes window-size 0))

    (define hash-heads : (Vectorof Index) (smart-make-vector hash-size no-compression?))
    (define hash-prevs : (Vectorof Index) (smart-make-vector window-size/2 no-compression?))
    (define hash-nil : Index (bytes-length window))
    (define prev-remained : Index 0)
    (define prev-hash : Index 0)

    (define raw-start : Index window-size/2)
    (define raw-end : Index window-size/2)

    ; for fallback to stored blocks if both static and dynamic ones are not smaller
    (define stored-start : Fixnum window-size/2)
    (define stored-count : Index 0)
    
    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym BFINAL) ; <=> (submit-huffman-symbol sym 0 BFINAL)
         (unsafe-vector*-set! lz77-block lz77-payload sym)
         (set! lz77-payload (unsafe-idx+ lz77-payload 1))
         (set! stored-count (unsafe-idx+ stored-count 1))
         (when (= lz77-payload lz77-blocksize)
           (void (lz77-block-flush BFINAL lz77-payload)))]
        [(distance span BFINAL)
         (unsafe-vector*-set! lz77-block lz77-payload span)
         (unsafe-vector*-set! lz77-dists lz77-payload distance)
         (set! lz77-payload (unsafe-idx+ lz77-payload 1))
         (set! stored-count (unsafe-idx+ stored-count span))
         (when (= lz77-payload lz77-blocksize)
           (void (lz77-block-flush BFINAL lz77-payload)))]))

    ;; for (dynamic) huffman tree
    (define literal-frequencies : (Vectorof Index) (smart-make-vector uplitcode (or no-compression? static-only?)))
    (define literal-codewords : (Mutable-Vectorof Index) (smart-make-vector uplitcode (or no-compression? static-only?)))
    (define literal-lengths : Bytes (smart-make-bytes uplitcode (or no-compression? static-only?)))

    (define distance-frequencies : (Vectorof Index) (smart-make-vector updistcode (or no-compression? static-only?)))
    (define distance-codewords : (Mutable-Vectorof Index) (smart-make-vector updistcode (or no-compression? static-only?)))
    (define distance-lengths : Bytes (smart-make-bytes updistcode (or no-compression? static-only?)))

    (define codeword-lengths : Bytes (smart-make-bytes (+ uplitcode updistcode) (or no-compression? static-only?)))
    (define codelen-symbols : Bytes (smart-make-bytes (+ uplitcode updistcode) (or no-compression? static-only?)))
    (define codelen-repeats : Bytes (smart-make-bytes (+ uplitcode updistcode) (or no-compression? static-only?)))
    (define codelen-lengths : Bytes (smart-make-bytes uplencode (or no-compression? static-only?)))
    (define codelen-frequencies : (Mutable-Vectorof Index) (smart-make-vector uplencode (or no-compression? static-only?)))
    (define codelen-codewords : (Mutable-Vectorof Index) (smart-make-vector uplencode (or no-compression? static-only?)))
    
    (define minheap : (Mutable-Vectorof Index) (smart-make-vector (+ uplitcode uplitcode) (or no-compression? static-only?)))
    (define prefab-counts : (Mutable-Vectorof Index) (smart-make-vector uplitcode (or no-compression? static-only?)))
    (define prefab-nextcodes : (Mutable-Vectorof Index) (smart-make-vector uplitcode (or no-compression? static-only?)))
    
    (define submit-huffman-symbol+frequency : LZ77-Submit-Symbol
      (case-lambda
        [(sym BFINAL) ; <=> (submit-huffman-symbol+frequency sym 0 BFINAL)
         (unsafe-vector*-set! literal-frequencies sym (unsafe-idx+ (unsafe-vector*-ref literal-frequencies sym) 1))
         (submit-huffman-symbol sym BFINAL)]
        [(distance span BFINAL)
         (let ([hspan (backref-span->huffman-symbol span)]
               [hdist (backref-distance->huffman-distance distance)])
           (unsafe-vector*-set! literal-frequencies hspan (unsafe-idx+ (unsafe-vector*-ref literal-frequencies hspan) 1))
           (unsafe-vector*-set! distance-frequencies hdist (unsafe-idx+ (unsafe-vector*-ref distance-frequencies hdist) 1))
           (submit-huffman-symbol distance span BFINAL))]))

    (define (construct-tree! [freqs : (Vectorof Index)] [codes : (Mutable-Vectorof Index)] [lengths : Bytes] [upcode : Index] [limit : Byte]) : (Values Byte Index)
      (huffman-frequencies->tree! freqs codes lengths upcode minheap prefab-nextcodes prefab-counts #:length-limit limit))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (huffman-calculate-stored-block-length-in-bits) : Natural
      (+ ($SHELL 'tellp)
         (+ 35 #| 3 + 16 * 2 |#
            (unsafe-idxlshift stored-count 3))))

    (define (huffman-calculate-static-block-length-in-bits) : Nonnegative-Fixnum
      (unsafe-fx+ 3
                  ; EOB is counted in literal-frequencies
                  (huffman-calculate-length-in-bits literal-frequencies distance-frequencies huffman-fixed-literal-lengths huffman-fixed-distance-lengths)))

    (define (huffman-calculate-dynamic-block-length-in-bits [hclen : Index]) : Nonnegative-Fixnum
      (unsafe-fx+ 17 #;(+ 3 5 5 4)

                  ; codelen codes lengths
                  (unsafe-idx+
                   (unsafe-idx+ (unsafe-idx* hclen 3)

                                (let codelensum ([len : Nonnegative-Fixnum 0]
                                                 [total : Nonnegative-Fixnum 0])
                                  (cond [(>= len uplencode) total]
                                        [else (codelensum (+ len 1)
                                                          (unsafe-idx+ total
                                                                       (unsafe-idx* (unsafe-vector*-ref codelen-frequencies len)
                                                                                    (+ (unsafe-bytes-ref codelen-lengths len)
                                                                                       (cond [(< len codelen-copy:2) 0]
                                                                                             [(= len codelen-copy:2) 2]
                                                                                             [(= len codelen-copy:3) 3]
                                                                                             [(= len codelen-copy:7) 7]
                                                                                             [else 0])))))])))
                  
                   ; EOB is counted in literal-frequencies
                   (huffman-calculate-length-in-bits literal-frequencies distance-frequencies literal-lengths distance-lengths))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (huffman-write-empty-block! [BFINAL : Boolean]) : Natural
      (PUSH-BITS (if BFINAL #b011 #b010) 3 #b111) ; empty static block
      (PUSH-BITS #;(vector-ref (force huffman-fixed-literal-codewords) EOB) EOB EOBbits) ; only when `(EOB & EOBbits)` is 0
      (SEND-BITS #:windup? BFINAL))
    
    (define (huffman-write-stored-block! [bsrc : Bytes] [BFINAL : Boolean] [start : Index] [end : Index]) : Natural
      (define size : Index (unsafe-idx- end start))
        
      (PUSH-BITS (if BFINAL #b001 #b000) 3 #b111)
      ($SHELL 'align)
      (PUSH-BITS size 16 #xFFFF)
      (PUSH-BITS (unsafe-uint16-not size) 16 #xFFFF)
      (PUSH-BITS bsrc start end #true)
      
      (SEND-BITS #:windup? BFINAL))

    (define (huffman-write-static-block! [l77src : (Vectorof Index)] [dists : (Vectorof Index)] [BFINAL : Boolean] [start : Index] [end : Index]) : Natural
      (PUSH-BITS (if BFINAL #b011 #b010) 3 #b111)

      (huffman-write-block! l77src dists
                            (force huffman-fixed-literal-codewords) (force huffman-fixed-distance-codewords)
                            huffman-fixed-literal-lengths huffman-fixed-distance-lengths
                            start end)
        
      (PUSH-BITS #;(vector-ref (force huffman-fixed-literal-codewords) EOB) EOB EOBbits) ; only when `(EOB & EOBbits)` is 0
      (SEND-BITS #:windup? BFINAL))

    (define (huffman-write-dynamic-block! [l77src : (Vectorof Index)] [dists : (Vectorof Index)] [BFINAL : Boolean] [start : Index] [end : Index]
                                          [hlit : Index] [hdist : Index] [hclen : Index] [sym-count : Index]) : Natural
      (PUSH-BITS (if BFINAL #b101 #b100) 3 #b111)

      (PUSH-BITS (unsafe-idx- hlit  literal-nbase)  5 #x1F)
      (PUSH-BITS (unsafe-idx- hdist distance-nbase) 5 #x1F)
      (PUSH-BITS (unsafe-idx- hclen codelen-nbase)  4 #x0F)

      (let push-codelen-lengths ([idx : Nonnegative-Fixnum 0])
        (when (< idx hclen)
          (PUSH-BITS (unsafe-bytes-ref codelen-lengths (unsafe-bytes-ref codelen-codes-order idx)) 3 #b111)
          (push-codelen-lengths (+ idx 1))))

      (let push-codeword-lengths ([idx : Nonnegative-Fixnum 0])
        (when (< idx sym-count)
          (define lensym : Index (unsafe-bytes-ref codelen-symbols idx))
          
          (PUSH-BITS (unsafe-vector*-ref codelen-codewords lensym) (unsafe-bytes-ref codelen-lengths lensym))

          (unless (< lensym codelen-copy:2)
            (define repeat : Index (unsafe-bytes-ref codelen-repeats idx))

            (cond [(= lensym codelen-copy:2) (PUSH-BITS (unsafe-idx- repeat codelen-min-match)   2 #b11)]
                  [(= lensym codelen-copy:3) (PUSH-BITS (unsafe-idx- repeat codelen-min-match)   3 #b111)]
                  [(= lensym codelen-copy:7) (PUSH-BITS (unsafe-idx- repeat codelen-min-match:7) 7 #x7F)]))

          (push-codeword-lengths (+ idx 1))))
      
      (huffman-write-block! l77src dists
                            literal-codewords distance-codewords
                            literal-lengths distance-lengths
                            start end)
      
      (PUSH-BITS (unsafe-vector*-ref literal-codewords EOB) (unsafe-bytes-ref literal-lengths EOB))
      (SEND-BITS #:windup? BFINAL))

    (define (huffman-write-block! [l77src : (Vectorof Index)] [dists : (Vectorof Index)]
                                  [literals : (Vectorof Index)] [distances : (Vectorof Index)]
                                  [literal-lengths : Bytes] [distance-lengths : Bytes]
                                  [start : Nonnegative-Fixnum] [end : Index]) : Void
      (let push-block ([idx : Nonnegative-Fixnum start])
        (when (< idx end)
          (define literal : Index (unsafe-vector*-ref l77src idx))
          (define distance : Index (unsafe-vector*-ref dists idx))
          
          (if (= distance 0)
              ; pure literals
              (PUSH-BITS (unsafe-vector*-ref literals literal) (unsafe-bytes-ref literal-lengths literal))
              
              ; <span, backward distance>, extra bits represent MSB-first machine (unsigned) integers
              (let ([hspan (backref-span->huffman-symbol literal)]
                    [hdist (backref-distance->huffman-distance distance)])
                (let* ([s-idx (unsafe-idx- hspan backref-span-offset)]
                       [extra (unsafe-bytes-ref huffman-backref-extra-bits s-idx)])
                  (PUSH-BITS (unsafe-vector*-ref literals hspan) (unsafe-bytes-ref literal-lengths hspan))
                  (when (> extra 0) (PUSH-BITS (unsafe-idx- literal (unsafe-vector*-ref huffman-backref-bases s-idx)) extra)))
                
                (let* ([extra (unsafe-bytes-ref huffman-distance-extra-bits hdist)])
                  (PUSH-BITS (unsafe-vector*-ref distances hdist) (unsafe-bytes-ref distance-lengths hdist))
                  (when (> extra 0) (PUSH-BITS (unsafe-idx- distance (unsafe-vector*-ref huffman-distance-bases hdist)) extra)))))
          
          (push-block (+ idx 1)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (raw-block-flush [BFINAL : Boolean] [flush-lz77? : Boolean]) : (U Void Natural)
      (cond [(not no-compression?)
             (when (or (> raw-end raw-start) (> prev-remained 0))
               (define-values (next-idx hash++)
                 (lz77-deflate #:hash-bits hash-bits #:hash-heads hash-heads #:hash-chain hash-prevs #:min-match min-match
                               window (if (not static-only?) submit-huffman-symbol+frequency submit-huffman-symbol)
                               strategy (unsafe-idx- raw-start prev-remained) raw-end prev-hash BFINAL hash-nil))

               (set! prev-remained (unsafe-idx- raw-end next-idx))
               (set! prev-hash hash++)

               ; the length of sliding window is assumed exactly multiple of length of the raw block
               (cond [(< raw-end window-size)
                      (set! raw-start raw-end)]
                     [else ; slide the window
                      (unsafe-bytes-copy! window 0 window window-size/2 window-size)
                      (lz77-slide-hash hash-heads hash-prevs window-size/2 hash-nil strategy)
                      (set! raw-start window-size/2)
                      (set! raw-end raw-start)
                      (set! stored-start (unsafe-fx- stored-start window-size/2))]))

             (when (or BFINAL flush-lz77?)
               (cond [(> lz77-payload 0) (lz77-block-flush BFINAL lz77-payload)]
                     [(and BFINAL) (huffman-write-empty-block! BFINAL)]))]
            [(> raw-end raw-start)
             (huffman-write-stored-block! window BFINAL raw-start raw-end)
             ; no need to maintain the history of the input for stored blocks,
             ; simply reset the end cursor to where it starts.
             (set! raw-end raw-start)]
            [(and BFINAL) (huffman-write-empty-block! BFINAL)]))

    (define (lz77-block-flush [BFINAL : Boolean] [payload : Index]) : Void
      ;;; NOTE
      ; The handy thing is that backref distances are allowed to cross block boundaries,
      ;   thus, the only thing to do here is to figure out a good way to encode current
      ;   lz77 block and feed the bitstream resulting bits to output.
      ; The `raw-block-flush` has the responsibility to feed the `lz77-deflate`
      ;   reasonable data and slide the window when necessary.

      (define-values (?stored-start non-dynamic-length)
        (cond [(and static-only?) (values #false 0)] ; no frequencies are gathered, `non-dynamic-bits` is meaningless
              [(< stored-start 0) (values #false (huffman-calculate-static-block-length-in-bits))]
              [else (let ([static-length (huffman-calculate-static-block-length-in-bits)]
                          [stored-length (huffman-calculate-stored-block-length-in-bits)])
                      (if (< static-length stored-length)
                          (values #false static-length)
                          (values stored-start stored-length)))]))

      (or (and (not static-only?)
               ;(huffman-patch-distances-for-buggy-inflator! distance-frequencies)
               (let-values ([(dist-maxlength hdist) (construct-tree! distance-frequencies distance-codewords distance-lengths updistcode codeword-length-limit)])
                 (and (>= hdist distance-nbase) ; it's impossible to compress a block with zero back references
                      (<= dist-maxlength codeword-length-limit)
                      (unsafe-vector*-set! literal-frequencies EOB 1) ; the `EOB` always testifies to `(>= hlit literal-nbase)`
                      (let-values ([(lit-maxlength hlit) (construct-tree! literal-frequencies literal-codewords literal-lengths uplitcode codeword-length-limit)])
                        (and (<= lit-maxlength codeword-length-limit)
                             ; combine literal alphabet and distance alphabet for better compressed codelen codes
                             (unsafe-bytes-copy! codeword-lengths 0 literal-lengths 0 hlit)
                             (unsafe-bytes-copy! codeword-lengths hlit distance-lengths 0 hdist)
                             (let*-values ([(nsym) (huffman-dynamic-lengths-deflate! codeword-lengths hlit hdist codelen-symbols codelen-repeats codelen-frequencies)]
                                           [(who cares) (construct-tree! codelen-frequencies codelen-codewords codelen-lengths uplencode uplenbits)]
                                           [(hclen) (huffman-dynamic-codelen-effective-count codelen-lengths)]
                                           [(dynamic-length) (huffman-calculate-dynamic-block-length-in-bits hclen)])
                               (and ;(< dynamic-length non-dynamic-length)
                                    (huffman-write-dynamic-block! lz77-block lz77-dists BFINAL 0 payload hlit hdist hclen nsym)
                                    (vector-fill! literal-frequencies 0)
                                    (vector-fill! distance-frequencies 0))))))))
          
          (if (index? ?stored-start)
              (huffman-write-stored-block! window BFINAL ?stored-start (unsafe-idx+ ?stored-start stored-count))
              (huffman-write-static-block! lz77-block lz77-dists BFINAL 0 payload)))

      (vector-fill! lz77-dists 0)
      
      (set! lz77-payload 0)
      (set! stored-start (unsafe-fx+ stored-start stored-count))
      (set! stored-count 0))
    
    (define (block-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define received : Integer (- end start))
      
      (when (> received 0)
        (let write-raw ([recv : Natural received]
                        [start : Natural start]
                        [available : Index (unsafe-idx- raw-blocksize (unsafe-idx- raw-end raw-start))])
          (if (>= recv available)
              (let ([start++ (unsafe-idx+ start available)])
                (unsafe-bytes-copy! window raw-end bs start start++)
                (set! raw-end (unsafe-idx+ raw-end available))
                (raw-block-flush #false #false)
                (when (> recv available)
                  (write-raw (unsafe-idx- recv available) start++ raw-blocksize)))
              (let ([end++ (unsafe-idx+ raw-end recv)])
                (unsafe-bytes-copy! window raw-end bs start end)
                (set! raw-end end++)))))

      (when (or non-block/buffered? #| non-block writing implies flushing |#
                (<= received 0) #| via `flush-port` or non-block writing with an empty bytes |#)
        (raw-block-flush #false #true))
      
      received)

    (define (block-close) : Void
      (raw-block-flush #true #true)
      
      (unless (not close-orig?)
        (close-output-port /dev/zipout)))
    
    (make-output-port name always-evt block-write block-close
                      #false port-always-write-evt #false
                      #false void
                      (λ [] (add1 ($SHELL 'aggregate)))
                      #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port (Option Natural))
                                         (Boolean #:window-bits Positive-Byte #:name (Option String) #:error-name Symbol #:commit? Boolean)
                                         Input-Port)
  (lambda [#:window-bits [winbits window-ibits] #:name [name #false] #:error-name [ename 'zip] #:commit? [commit? #false]
           /dev/zipin csize [close-orig? #false]]
    (define BTYPE : (Option Symbol) #false)
    (define BFINAL : Boolean #false)
    
    (define literal-alphabet : Huffman-Alphabet (force huffman-fixed-literal-alphabet))
    (define distance-alphabet : Huffman-Alphabet (force huffman-fixed-distance-alphabet))
    (define literal-maxlength : Byte huffman-fixed-literal-maxlength)
    (define distance-maxlength : Byte huffman-fixed-distance-maxlength)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define dynamic-literal-alphabet : Huffman-Alphabet (huffman-make-alphabet uplitcode #:max-bitwidth upbits))
    (define dynamic-distance-alphabet : Huffman-Alphabet (huffman-make-alphabet updistcode #:max-bitwidth upbits))
    (define codelen-alphabet : Huffman-Alphabet (huffman-make-alphabet uplencode #:max-bitwidth uplenbits))
    (define codelen-lengths : Bytes (make-bytes uplencode 0))
    (define codeword-lengths : Bytes (make-bytes (+ uplitcode updistcode) 0))
    (define prefab-indices : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define prefab-counts : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define prefab-codes : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    
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
                      [(#b00) (huffman-begin-read-stored-block! BFINAL?) 'stored]
                      [(#b01) (huffman-begin-read-static-block! BFINAL?) 'static]
                      [(#b10) (huffman-begin-read-dynamic-block! BFINAL?) 'dynamic]
                      [else (throw-check-error /dev/blkin ename "unknown deflate block type: ~a" (~binstring BTYPE 2))])
                    BFINAL?))
          (values 'EOB #true)))

    (define (huffman-begin-read-stored-block! [BFINAL? : Boolean]) : Any
      ($SHELL 'align)
      
      (unless (FEED-BITS 32)
        (throw-eof-error /dev/blkin 'huffman-begin-stored-block!))
      
      (set! payload (PEEK-BITS 16 #xFFFF))

      (let ([chksum (PEEK-BITS 16 #xFFFF 16)])
        (unless (= (unsafe-uint16-not payload) chksum)
          (throw-check-error /dev/blkin ename "stored[~a]: invalid block length: ~a ~a"
                             (if (not BFINAL?) #b0 #b1) (~binstring payload 16) (~binstring chksum 16))))

      (FIRE-BITS 32))
    
    (define (huffman-begin-read-static-block! [BFINAL? : Boolean]) : Any
      (unless (eq? BTYPE 'static)
        (set! literal-alphabet (force huffman-fixed-literal-alphabet))
        (set! distance-alphabet (force huffman-fixed-distance-alphabet))
        (set! literal-maxlength huffman-fixed-literal-maxlength)
        (set! distance-maxlength huffman-fixed-distance-maxlength)))

    (define (huffman-begin-read-dynamic-block! [BFINAL? : Boolean]) : Any
      (unless (FEED-BITS 14)
        (throw-eof-error /dev/blkin 'huffman-begin-dynamic-block!))
      
      (define hlit  : Index (unsafe-idx+ (PEEK-BITS 5 #x1F) literal-nbase))
      (define hdist : Index (+ (PEEK-BITS 5 #x1F 5) distance-nbase))
      (define hclen0 : Byte (PEEK-BITS 4 #x0F 10))
      (define hclen : Index (+ hclen0 codelen-nbase))

      (FIRE-BITS 14)
      
      ; note that bits of the header are extremely compact,
      ;   so that they never exceed the permissive upper boundaries.
      ; also recall that the last two codes of both literals and distances are actually ununsed,
      ;   nonetheless, some buggy implementations might use them.
      ; thus, we are not going to check their validity.

      ;; WARNING
      ; After feeding and firing 3 + 14 bits, there are still 7 bits left in the bitstream before feeding more,
      ;   and the codelen codes will consume at most 19 * 3 = 57 bits, in which case the actual bits would be
      ;   7 + roundup(57 - 7) = 63 bits, which seems safe in a 64-bit machine. However, Racket supports big
      ;   integer as the first-class number by reserving the leading 1 or 3 bits (depending on the variant)
      ;   to distinguish big integers and mechine integers (a.k.a fixnums).
      ; Operating bits one by one is usually inefficient, our bitstream facility is therefore in the charge of
      ;   `racket/unsafe/ops`. That is to say, we have to keep in mind that the feeding bits shouldn't cause
      ;   the overflow of fixnum, or the momery would be ruined clandestinely.
      ; Thus, the reading of codelen codes must be splitted into two (or more) steps. Yes, another alternative
      ;   would be to do FEED-PEEK-FIRE operations on every code, but that is less efficient as it sounds.
      ; Lucky, the specification says the number of codelen codes must be at least 4, that's a reasonable hint.
      ;
      ;; NOTE
      ; Above discussion is actually in a specific case, but a huffman block doesn't have to start at the byte
      ;   boundary. Nonetheless, feeding another 7 bits is always heuristic.

      (when (< hclen uplencode) (bytes-fill! codelen-lengths 0)) ; the omited codelen codes are 0s
      (huffman-read-codelen-codes! 0 codelen-nbase codelen-nbase) ; load the first 4 codelen codes, it would be at most 3 + roundup(12 - 3) = 18 bits
      (huffman-read-codelen-codes! codelen-nbase hclen hclen0)    ; load the rest, it would be at most 4 + roundup(45 - 4) = 52 bits
      
      ; the lengths of codewords of real literals and distances are also encoded as huffman codes,
      ; the code length codes are just what encode those lengths.
      (huffman-alphabet-canonicalize! codelen-alphabet codelen-lengths 0 uplencode prefab-indices prefab-counts prefab-codes
                                      #:on-error (λ [[len : Index]] (throw-check-error /dev/blkin ename "dynamic[~a]: codelen length overflow: ~a"
                                                                                       (if (not BFINAL?) #b0 #b1) len)))
    
      (let ([N (unsafe-idx+ hlit hdist)])
        ; the lengths 16, 17, and 18 in codelen lengths are not real lengths of codelen codes,
        ;   but indicators for repeating previous or zero lengths, and the repetition might run
        ;   across the boundary of lengths of literals and distances.
        ; thus, lengths of literals and distances have to be read in one batch.
        (bytes-fill! codeword-lengths 0) ; for values 17 and 18 where tons of 0s are copied.
        (let read-codeword-length : Void ([code-idx : Nonnegative-Fixnum 0]
                                          [prev-len : Index 0])
          (when (< code-idx N)
            (define lensym : Index (read-huffman-symbol codelen-alphabet uplenbits uplencode BFINAL? 'dynamic 'length))

            (cond [(< lensym codelen-copy:2) (unsafe-bytes-set! codeword-lengths code-idx lensym) (read-codeword-length (+ code-idx 1) lensym)]
                  [(= lensym codelen-copy:3) (read-codeword-length (read-huffman-repetition-times code-idx 3 #b111 codelen-min-match N BFINAL?) 0)]
                  [(= lensym codelen-copy:7) (read-codeword-length (read-huffman-repetition-times code-idx 7 #x7F codelen-min-match:7 N BFINAL?) 0)]
                  [else ; codelen-copy:2 ; to repeat previous length 3 - 6 times, determined by next 2 bits
                   (let ([idx++ (read-huffman-repetition-times code-idx 2 #b11 codelen-min-match N BFINAL?)])
                     (cond [(= prev-len 0) (throw-check-error /dev/blkin ename "dynamic[~a]: previous length shouldn't be zero" (if (not BFINAL?) #b0 #b1))]
                           [else (let copy-prev ([idx : Nonnegative-Fixnum code-idx])
                                   (cond [(< idx idx++) (unsafe-bytes-set! codeword-lengths idx prev-len) (copy-prev (unsafe-fx+ idx 1))]
                                         [else (read-codeword-length idx++ prev-len)]))]))])))

        (huffman-alphabet-canonicalize! dynamic-literal-alphabet codeword-lengths 0 hlit prefab-indices prefab-counts prefab-codes
                                        #:on-error (λ [[len : Index]] (throw-check-error /dev/blkin ename "dynamic[~a]: literal length overflow: ~a"
                                                                                         (if (not BFINAL?) #b0 #b1) len)))
        
        (huffman-alphabet-canonicalize! dynamic-distance-alphabet codeword-lengths hlit N prefab-indices prefab-counts prefab-codes
                                        #:on-error (λ [[len : Index]] (throw-check-error /dev/blkin ename "dynamic[~a]: distance length overflow: ~a"
                                                                                         (if (not BFINAL?) #b0 #b1) len))))

      (unless (eq? BTYPE 'dynamic)
        (set! literal-alphabet dynamic-literal-alphabet)
        (set! distance-alphabet dynamic-distance-alphabet)
        (set! literal-maxlength upbits)
        (set! distance-maxlength upbits)))

    (define (huffman-read-codelen-codes! [start : Byte] [end : Index] [count : Byte]) : Void
      (let ([nbits (unsafe-b* count 3)])
        (unless (FEED-BITS nbits)
          (throw-eof-error /dev/blkin 'read-codelen-lengths!))
        
        (let read-codelen-lengths! ([idx : Nonnegative-Fixnum start]
                                    [skip : Byte 0])
          (when (< idx end)
            (unsafe-bytes-set! codelen-lengths (unsafe-bytes-ref codelen-codes-order idx) (PEEK-BITS 3 #b111 skip))
            (read-codelen-lengths! (+ idx 1) (unsafe-b+ skip 3))))
        
        (FIRE-BITS nbits)))

    (define (huffman-error-handler [fmt : String] [BFINAL? : Boolean]) : (-> Index Nothing)
      (λ [[len : Index]]
        (throw-check-error /dev/blkin ename fmt
                           (if (not BFINAL?) #b0 #b1) len)))

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
        (let lazy-extract : (Values Nonnegative-Fixnum Index) ([supply : Nonnegative-Fixnum payload]
                                                               [widx : Index payload-idx])
          (cond [(>= supply request) (values supply widx)]
                [else (let ([misc (read-huffman-symbol literal-alphabet literal-maxlength strict-uplitcode BFINAL? type 'literal)])
                        (cond [(< misc EOB) ; pure literals
                               (let-values ([(widx++ ipos supply++) (huffman-try-slide-window widx supply 1)])
                                 (lz77-inflate-into window ipos misc)
                                 (lazy-extract supply++ widx++))]
                              [(> misc EOB) ; <span, backward distance>s, extra bits represent MSB-first machine (unsigned) integers
                               (let* ([span-idx (unsafe-idx- misc backref-span-offset)]
                                      [span (read-huffman-extra-datum span-idx huffman-backref-bases huffman-backref-extra-bits)]
                                      [hdist (read-huffman-symbol distance-alphabet distance-maxlength strict-updistcode BFINAL? type 'distance)]
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

    (define (read-huffman-symbol [table : Huffman-Alphabet] [maxlength : Byte] [upcodes : Index] [BFINAL? : Boolean] [btype : Any] [ctype : Any]) : Index
      (define far-away-from-eof? : Boolean (FEED-BITS upsymbits)) ; for dynamic huffman encoding, the maximum bit length would be 15 + 13, rounded up to 32  
        
      (let-values ([(symbol-code code-length) (huffman-symbol-lookup table (PEEK-BITS) maxlength)])
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

        (when (not far-away-from-eof?)
          ; Only to check if the current symbol is still before EOF.
          ;   If not we should have failed to touch the EOB, in which case
          ;   we are encountering either a malicious source or an internal error on our own side.
          ; NOTE that the length of EOB might vary among dynamic blocks.
          (unless (FEED-BITS code-length) 
            (throw-eof-error /dev/blkin 'read-huffman-symbol
                             "~a[~a]: unexpected end of huffman stream at ~a symbol '~a'[~a]"
                             btype (if (not BFINAL?) #b0 #b1) ctype
                             (integer->char symbol-code) (~binstring symbol-code code-length))))
        
        (FIRE-BITS code-length)

        symbol-code))

    ; the next two functions are invoked as extra subroutines of the above one
    ; that is, no need to feed bits again since the above one guarantees bits adequate.
    (define (read-huffman-extra-datum [idx : Index] [base-bits : (Immutable-Vectorof Index)] [extra-bits : Bytes]) : Index
      (define base : Index (unsafe-vector*-ref base-bits idx))
      (define extra : Byte (unsafe-bytes-ref extra-bits idx))

      (cond [(= extra 0) base]
            [else (begin0 (unsafe-idx+ base (PEEK-BITS extra))
                          (FIRE-BITS extra))]))

    (define (read-huffman-repetition-times [idx : Index] [bitsize : Byte] [bitmask : Byte] [nbase : Byte] [N : Index] [BFINAL? : Boolean]) : Index
      (define repeat : Byte (unsafe-b+ (PEEK-BITS bitsize bitmask) nbase))
      (define idx++ : Nonnegative-Fixnum (+ idx repeat))
      
      (when (> idx++ N)
        (throw-check-error /dev/blkin ename
                           "dynamic[~a]: repeated codes overstep the boundary: ~a (only ~a left)"
                           (if (not BFINAL?) #b0 #b1) repeat (- N idx)))

      (FIRE-BITS bitsize)
      idx++)

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
(define vector-placeholder : (Mutable-Vectorof Index) (make-vector 0))

(define smart-make-bytes : (->* (Natural Boolean) (Byte) Bytes)
  (lambda [size unused? [defval 0]]
    (cond [(not unused?) (make-bytes size defval)]
          [else #""])))

(define smart-make-vector : (->* (Natural Boolean) (Index) (Mutable-Vectorof Index))
  (lambda [size unused? [defval 0]]
    (cond [(not unused?) (make-vector size defval)]
          [else vector-placeholder])))
