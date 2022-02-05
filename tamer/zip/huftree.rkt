#lang typed/racket/base

(provide (all-defined-out))

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
    (define frequencies : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define lengths : Bytes (make-bytes uplitcode 0))
    (define huffman-tree : (Mutable-Vectorof Index) (make-vector (+ uplitcode uplitcode) 0))
    (define codewords : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define nextcodes : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define counts : (Mutable-Vectorof Index) (make-vector uplitcode 0))
    (define symbol-indices : (Mutable-Vectorof Index) (make-vector uplitcode 0))

    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym final?)
         (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1))]
        [(distance span final?)
         (let* ([sym (backref-span->huffman-symbol span)])
           (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1)))]))

    (lz77-deflate txt submit-huffman-symbol (assert (zip-name->maybe-strategy 'identity)))
    
    (define heapsize : Index
      (time** #:title 'heapify
              (let ([heapsize (huffman-refresh-minheap! frequencies huffman-tree)])
                (huffman-minheapify! huffman-tree heapsize)
                heapsize)))
    
    (define heap? : (U Boolean Natural)
      (let sub-okay? : (U Boolean Natural) ([i 0])
        (or (>= i heapsize)
            (let* ([self-freq (vector-ref huffman-tree (vector-ref huffman-tree i))]
                   [left-freq (sub-okay? (+ (* i 2) 1))]
                   [right-freq (sub-okay? (+ (* i 2) 2))])
              (and left-freq right-freq
                   (or (boolean? left-freq) (<= self-freq left-freq))
                   (or (boolean? right-freq) (<= self-freq right-freq))
                   self-freq)))))

    (for ([idx (in-range heapsize)])
      (define ptr (vector-ref huffman-tree idx))
      (define bitsize (vector-ref huffman-tree ptr))
      (define symbol (- ptr uplitcode))

      (echof #:fgcolor (if (not heap?) 'red 'green) "(~a . ~a)~n"
             (codesymbol->visual-value symbol) bitsize))

    (define maxlength
      (time** #:title 'treefy
              (huffman-minheap-treefy! huffman-tree heapsize)
              (let-values ([(_ maxlength _ec) (huffman-minheap-count-lengths! huffman-tree heapsize lengths)])
                maxlength)))

    (printf "max length: ~a~n" maxlength)
    (printf "lengths: ~a~n" (bytes->list lengths))

    (time** #:title 'huffman-codewords-canonicalize!
            (huffman-codewords-canonicalize! codewords lengths maxlength 0 uplitcode nextcodes counts))
    
    (display-codeword codewords lengths)

    (let ([alphabet (huffman-make-alphabet #:max-bitwidth maxlength)])
      (time** #:title 'huffman-alphabet-canonicalize!
              (huffman-alphabet-canonicalize! alphabet lengths 0 uplitcode symbol-indices counts nextcodes))
      
      (display-alphabet alphabet))))

(define fixed-run : (-> Void)
  (lambda []
    (display-codeword (time** #:title 'huffman-fixed-literal-codewords (force huffman-fixed-literal-codewords)) huffman-fixed-literal-lengths)
    (display-codeword (time** #:title 'huffman-fixed-distance-codewords (force huffman-fixed-distance-codewords)) huffman-fixed-distance-lengths)
    (display-alphabet (time** #:title 'huffman-fixed-literal-alphabet (force huffman-fixed-literal-alphabet)))
    (display-alphabet (time** #:title 'huffman-fixed-distance-alphabet (force huffman-fixed-distance-alphabet)))))

(define display-codeword : (-> (Vectorof Index) Bytes Void)
  (lambda [codewords lengths]
    (for ([codeword (in-vector codewords)]
          [bitsize (in-bytes lengths)]
          [symbol (in-naturals)]
          #:when (> bitsize 0))
      (displayln (list (codesymbol->visual-value symbol)
                       (~binstring (bits-reverse-uint16 codeword bitsize) bitsize)
                       '=> (~binstring codeword bitsize))))))

(define display-alphabet : (-> Huffman-Alphabet Void)
  (lambda [tbl]
    (define symbols : (HashTable Any (Listof (Pairof Any Any))) (make-hasheq))
    (define cheat-bwidth (huffman-alphabet-cheat-bwidth tbl))
      
    (for ([cheatcode (in-vector (huffman-alphabet-cheatsheet tbl))]
          [codeword (in-naturals)]
          #:when (> cheatcode 0))
      (define symbol (bitwise-and cheatcode (huffman-alphabet-symbol-mask tbl)))
      (define bitsize (arithmetic-shift cheatcode (- (huffman-alphabet-max-bwidth tbl))))
      (define symkey (codesymbol->visual-value symbol))
      (define symval (cons (~binstring codeword cheat-bwidth)
                           (~binstring (bitwise-bit-field codeword 0 bitsize))))

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
    (for ([offset (in-vector (huffman-alphabet-head-offsets tbl))]
          [sentinel (in-vector (huffman-alphabet-sentinels tbl))]
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
