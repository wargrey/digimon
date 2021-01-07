#lang typed/racket/base

(require racket/list)
(require racket/file)
(require racket/path)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/format)
(require digimon/debug)
(require digimon/echo)

(require "../digitama/bintext/lz77.rkt")
(require "../digitama/bintext/zipconfig.rkt")
(require "../digitama/unsafe/ops.rkt")
(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option lz77-flags #: Lz77-Flags
  #:program 'lz77
  #:args [file.zip]

  #:once-each
  [[(#\B)   #:=> cmdopt-string+>byte hash-Bits #: Positive-Byte "hash Bits"]
   [(#\f)   #:=> cmdopt-string->index farthest #: Index         "farthest"]
   [(#\m)   #:=> cmdopt-string+>byte min-match #: Positive-Byte "minimum match"]
   [(#\v)   #:=> lz77-verbose                                   "run with verbose messages"]])

(define lz77-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-run : (-> Bytes (U ZIP-Strategy (Listof ZIP-Strategy)) Positive-Byte (Option Positive-Byte) Index Void)
  (lambda [txt strategies bits min-match farthest]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof (U Byte (Pairof Index Index))) (make-vector rsize 0))

    (define record-codeword : LZ77-Select-Codeword
      (case-lambda
        [(codeword dest-idx)
         (vector-set! magazine dest-idx codeword)]
        [(pointer size dest-idx)
         (vector-set! magazine dest-idx (cons pointer size))]))

    (define display-codeword : LZ77-Select-Codeword
      (case-lambda
        [(codeword dest-idx)
         (record-codeword codeword dest-idx)
         (display (integer->char codeword))
         (flush-output (current-output-port))]
        [(pointer size dest-idx)
         (record-codeword pointer size dest-idx)
         (display (cons pointer size))
         (flush-output (current-output-port))]))

    (define summaries : (Listof (Listof String))
      (for/list ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
        (define desc : String (strategy->description strategy))
        
        (when (lz77-verbose) (printf "==> [strategy: ~a]~n" desc))

        (collect-garbage 'major)
        (collect-garbage 'major)
        (collect-garbage 'major)

        (define-values (results cpu real gc)
          (time-apply (λ [] (let ([csize (lz77-deflate #:hash-bits bits #:min-match min-match #:farthest farthest
                                                       txt (if (lz77-verbose) display-codeword record-codeword) strategy)])
                              (define-values (?txt total) (lz77-inflate (in-vector magazine 0 csize)))
                              (list csize ?txt)))
                      null))

        (when (lz77-verbose) (newline))

        (define csize : Index (caar results))
        (define ?txt : Bytes (cadar results))
        
        (let ([ok? (bytes=? txt ?txt)])
          (when (and (not ok?) (lz77-verbose))
            (displayln '==>)
            (displayln ?txt))
          
          (list (if (not ok?) "F" "T")
                desc (zip-size csize) (zip-cfactor csize rsize 2)
                (~gctime cpu) (~gctime real) (~gctime gc)))))

    (when (lz77-verbose) (printf "=============~n"))

    (when (pair? summaries)
      (let ([widths (text-column-widths summaries)])
        (for ([summary (in-list summaries)])
          (define color : Term-Color (if (string-ci=? (car summary) "T") 'green 'red))
          
          (for ([col (in-list summary)]
                [wid (in-list widths)]
                [idx (in-naturals)])
            (when (> idx 0) (display #\space))
            
            (let ([numerical? (memq (string-ref col (sub1 (string-length col))) (list #\% #\B))])
              (echof #:fgcolor color (~a col #:min-width wid #:align (if numerical? 'right 'left)))))

          (newline))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-lz77-flags argument-list #:help-output-port (current-output-port)))
    (define src.txt (λargv))

    (define special-strategies : (Listof ZIP-Strategy) (map zip-special-preference '(huffman-only plain)))
    (define default-strategies : (Listof ZIP-Strategy) (map zip-default-preference (range 1 10)))
    (define backward-strategies : (Listof ZIP-Strategy) (map zip-backward-preference (range 1 10)))
    (define rle-strategies : (Listof ZIP-Strategy) (map zip-run-preference (range 1 5)))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (time-apply* (λ [] (let ([tracer (thread (make-zip-log-trace))])
                                 (printf "~a {hash Bits: ~a} {minimum match: ~a} {farthest: ~a}~n"
                                         (if (file-exists? src.txt) (simple-form-path src.txt) src.txt)
                                         (lz77-flags-B options) (lz77-flags-m options) (lz77-flags-f options))
                                 
                                 (lz77-run (if (file-exists? src.txt) (file->bytes src.txt) (string->bytes/utf-8 src.txt))
                                           (append special-strategies default-strategies backward-strategies rle-strategies)
                                           (or (lz77-flags-B options) lz77-default-hash-bits)
                                           (lz77-flags-m options)
                                           (or (lz77-flags-B options) lz77-default-farthest))
                                 
                                 (dtrace-datum-notice eof)
                                 (thread-wait tracer))))))))

(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (lz77-verbose) 'trace 'info))))

(define strategy->description : (-> ZIP-Strategy String)
  (lambda [s]
    (define name : Symbol (zip-strategy-name s))
    
    (cond [(zip-default-strategy? s) (format "~a:~a" name (zip-deflation-config-level (zip-default-strategy-config s)))]
          [(zip-backward-strategy? s) (format "~a:~a" name (zip-deflation-config-level (zip-backward-strategy-config s)))]
          [(zip-run-strategy? s) (format "~a:~a" name (zip-run-strategy-length s))]
          [else (symbol->string name)])))

(module+ main
  (main (current-command-line-arguments)))
