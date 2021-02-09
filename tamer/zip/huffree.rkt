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
(define tree-run : (-> Bytes Void) ; WARNING: the heap is 0-based
  (lambda [txt]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))
    (define frequencies : (Mutable-Vectorof Index) (make-vector upcodes 0))
    (define canonical-heap : (Mutable-Vectorof Index) (make-vector (+ upcodes upcodes) 0))

    (define submit-huffman-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym d-idx)
         (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1))]
        [(distance span d-idx)
         (let* ([sym (backref-span->huffman-symbol span)])
           (unsafe-vector*-set! frequencies sym (unsafe-idx+ (unsafe-vector*-ref frequencies sym) 1)))]))

    (lz77-deflate txt submit-huffman-symbol (assert (zip-name->maybe-strategy 'identity)))
        
    (collect-garbage*)

    (define n : Index
      (time-apply*
       (λ [] (let ([n (huffman-refresh-minheap! frequencies canonical-heap)])
               (huffman-minheapify! canonical-heap n)
               n))
       #true))
    
    (define heap-okay? : (U Boolean Natural)
      (let sub-okay? : (U Boolean Natural) ([i 0])
        (or (>= i n)
            (let* ([self-freq (vector-ref canonical-heap (vector-ref canonical-heap i))]
                   [left-freq (sub-okay? (+ (* i 2) 1))]
                   [right-freq (sub-okay? (+ (* i 2) 2))])
              (and left-freq right-freq
                   (or (boolean? left-freq) (<= self-freq left-freq))
                   (or (boolean? right-freq) (<= self-freq right-freq))
                   self-freq)))))

    (if (not heap-okay?)
        (echof #:fgcolor 'red "not a heap~n")
        (echof #:fgcolor 'green "heap ready~n"))

    (collect-garbage*)
    
    (time-apply*
     (λ [] (begin (huffman-minheap-treefy! canonical-heap n)
                  (huffman-minheap-count-lengths! canonical-heap n)))
     #true)
    
    (for ([codeword (in-range upcodes)]
          [len (in-vector canonical-heap upcodes)]
          #:when (> len 0))
      (displayln (cons codeword len)))))

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
