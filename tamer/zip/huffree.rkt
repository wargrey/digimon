#lang typed/racket/base

(require racket/file)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/debug)
(require digimon/echo)

(require digimon/digitama/bintext/lz77)
(require digimon/digitama/bintext/huffman)
(require digimon/digitama/bintext/zipconfig)
(require digimon/digitama/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option tree-flags #: Tree-Flags
  #:program 'huffree
  #:args [file.zip]

  #:once-each
  [[(#\v) #:=> tree-verbose "run with verbose messages"]])

(define tree-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree-run : (-> Bytes Void)
  (lambda [txt]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))
    (define freq-offset : Index (unsafe-idx+ 1 upcodes))
    (define frequencies : (Mutable-Vectorof Index) (make-vector upcodes 0))
    (define cw-lengths : (Mutable-Vectorof Index) (make-vector upcodes 0))
    (define canonical-heap : (Mutable-Vectorof Index) (make-vector (+ 1 upcodes upcodes) 0))

    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx)
         (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1))]
        [(distance span d-idx)
         (let* ([sym (backref-span->huffman-symbol span)])
           (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1)))]))

    (lz77-deflate txt submit-huffman-symbol (assert (zip-name->maybe-strategy 'default)))
        
    (collect-garbage*)

    (time-apply* (λ [] (huffman-heapify frequencies canonical-heap)) #true)

    (define heap-okay? : Boolean
      (for/and : Boolean ([i (in-range 1 (quotient freq-offset 2))])
        (define self-freq : Index (vector-ref canonical-heap (vector-ref canonical-heap i)))
        (and (<= self-freq (vector-ref canonical-heap (vector-ref canonical-heap (* i 2))))
             (<= self-freq (vector-ref canonical-heap (vector-ref canonical-heap (+ (* i 2) 1)))))))

    (if (not heap-okay?)
        (echof #:fgcolor 'red "not a heap~n")
        (echof #:fgcolor 'green "heap ready~n"))
    
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-tree-flags argument-list #:help-output-port (current-output-port)))
    (define src.txt (λargv))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (time-apply* (λ [] (let ([tracer (thread (make-zip-log-trace))])
                                 (tree-run (if (file-exists? src.txt) (file->bytes src.txt) (string->bytes/utf-8 src.txt)))
                                 
                                 (dtrace-datum-notice eof)
                                 (thread-wait tracer))))))))

(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (tree-verbose) 'trace 'info))))

(module+ main
  (main (current-command-line-arguments)))
