#lang typed/racket/base

(require racket/file)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/format)
(require digimon/bitstream)
(require digimon/debug)
(require digimon/echo)

(require digimon/digitama/bintext/lz77)
(require digimon/digitama/bintext/huffman)
(require digimon/digitama/bintext/zipconfig)
(require digimon/digitama/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option tree-flags #: Tree-Flags
  #:program 'huftree
  #:args [filename]

  #:once-each
  [[(#\v) #:=> tree-verbose "run with verbose messages"]])

(define tree-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree-run : (-> Bytes Void) ; WARNING: the heap is 0-based
  (lambda [txt]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))
    (define frequencies : (Mutable-Vectorof Index) (make-vector upcodes 0))
    (define huffman-tree : (Mutable-Vectorof Index) (make-vector (+ upcodes upcodes) 0))
    (define codewords : (Mutable-Vectorof Index) (make-vector upcodes 0))
    (define temp-codes : (Mutable-Vectorof Index) (make-vector upcodes 0))

    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx)
         (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1))]
        [(distance span d-idx)
         (let* ([sym (backref-span->huffman-symbol span)])
           (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1)))]))

    (lz77-deflate txt submit-huffman-symbol (assert (zip-name->maybe-strategy 'identity)))
        
    (define n : Index
      (time** #:title 'heapify
              (let ([n (huffman-refresh-minheap! frequencies huffman-tree)])
                (huffman-minheapify! huffman-tree n)
                n)))
    
    (define heap? : (U Boolean Natural)
      (let sub-okay? : (U Boolean Natural) ([i 0])
        (or (>= i n)
            (let* ([self-freq (vector-ref huffman-tree (vector-ref huffman-tree i))]
                   [left-freq (sub-okay? (+ (* i 2) 1))]
                   [right-freq (sub-okay? (+ (* i 2) 2))])
              (and left-freq right-freq
                   (or (boolean? left-freq) (<= self-freq left-freq))
                   (or (boolean? right-freq) (<= self-freq right-freq))
                   self-freq)))))

    (for ([idx (in-range n)])
      (define ptr (vector-ref huffman-tree idx))
      (define bitsize (vector-ref huffman-tree ptr))
      (define symbol (- ptr upcodes))

      (echof #:fgcolor (if (not heap?) 'red 'green) "(~a . ~a)~n"
             (codesymbol->visual-value symbol) bitsize))

    (define maxlength : Byte
      (time** #:title 'treefy
              (huffman-minheap-treefy! huffman-tree n)
              (huffman-minheap-count-lengths! huffman-tree n)))

    (printf "max length: ~a~n" maxlength)

    (time** #:title 'huffman-codewords-canonicalize!
            (huffman-codewords-canonicalize! codewords huffman-tree maxlength temp-codes upcodes))
    
    (display-codeword codewords huffman-tree upcodes)

    (let ([lookup (huffman-make-lookup-table #:fast-lookup-bits (min 8 maxlength) #:max-bitwidth maxlength)])
      (time** #:title 'huffman-lookup-table-canonicalize!
              (huffman-lookup-table-canonicalize! lookup huffman-tree maxlength temp-codes upcodes))
      
      (display-lookup-table lookup))))

(define fixed-run : (-> Void)
  (lambda []
    (display-codeword (time** #:title 'huffman-fixed-literal-codewords (force huffman-fixed-literal-codewords)) huffman-fixed-literal-lengths)
    (display-codeword (time** #:title 'huffman-fixed-distance-codewords (force huffman-fixed-distance-codewords)) huffman-fixed-distance-lengths)
    (display-lookup-table (time** #:title 'huffman-fixed-literal-lookup-table (force huffman-fixed-literal-lookup-table)))
    (display-lookup-table (time** #:title 'huffman-fixed-distance-lookup-table (force huffman-fixed-distance-lookup-table)))))

(define display-codeword : (->* ((Vectorof Index) (Vectorof Index)) (Index) Void)
  (lambda [codewords lengths [offset 0]]
    (for ([codeword (in-vector codewords)]
          [bitsize (in-vector lengths offset)]
          [symbol (in-naturals)]
          #:when (> bitsize 0))
      (displayln (list (codesymbol->visual-value symbol)
                       (~binstring (bits-reverse-uint16 codeword bitsize) bitsize)
                       '=> (~binstring codeword bitsize))))))

(define display-lookup-table : (-> Huffman-Lookup-Table Void)
  (lambda [tbl]
    (define symbols : (HashTable Any (Listof (Pairof Any Any))) (make-hasheq))
    (define cheat-bwidth (huffman-lookup-table-cheat-bwidth tbl))
      
    (for ([cheatcode (in-vector (huffman-lookup-table-cheatsheet tbl))]
          [codeword (in-naturals)]
          #:when (> cheatcode 0))
      (define symbol (bitwise-and cheatcode (huffman-lookup-table-symbol-mask tbl)))
      (define bitsize (arithmetic-shift cheatcode (- (huffman-lookup-table-symbol-bwidth tbl))))
      (define symkey (codesymbol->visual-value symbol))
      (define symval (cons (~binstring codeword cheat-bwidth)
                           (~binstring (bitwise-bit-field codeword 0 bitsize) bitsize)))

      (hash-set! symbols symkey
                 (cons symval
                       (hash-ref symbols symkey
                                 (inst list (Pairof Any Any))))))

    (displayln "cheatsheet:")
    (for ([(key value) (in-hash symbols)])
      (when (pair? value)
        (displayln (list (~s key) (cdar value)
                         (for/fold ([pads : (Listof Any) null])
                                   ([val (in-list value)])
                           (cons (car val) pads))))))

    (displayln "offsets + sentinels:")
    (for ([offset (in-vector (huffman-lookup-table-head-offsets tbl))]
          [sentinel (in-vector (huffman-lookup-table-sentinels tbl))]
          [length (in-naturals 1)])
      (displayln (list length offset (~binstring sentinel))))))

(define codesymbol->visual-value : (-> Integer Any)
  (lambda [sym]
    (let ([ch (integer->char sym)])
      (if (char-graphic? ch) ch sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-tree-flags argument-list #:help-output-port (current-output-port)))
    (define src.txt (λargv))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (time* (let ([tracer (thread (make-zip-log-trace))])
                     (define txt (if (file-exists? src.txt) (file->bytes src.txt) (string->bytes/utf-8 src.txt)))
                     
                     (if (bytes=? txt #"") (fixed-run) (tree-run txt))
                     
                     (dtrace-datum-notice eof)
                     (thread-wait tracer)))))))

(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (tree-verbose) 'trace 'info))))

(module+ main
  (main (current-command-line-arguments)))
