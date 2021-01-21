#lang typed/racket/base

(require racket/list)
(require racket/file)
(require racket/path)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/format)
(require digimon/number)
(require digimon/debug)
(require digimon/echo)

(require "../digitama/bintext/lz77.rkt")
(require "../digitama/bintext/zipconfig.rkt")
(require "../digitama/unsafe/ops.rkt")
(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-strategy : (-> Symbol String String String (List Symbol Byte Index))
  (lambda [option str.name str.#0 str.#n]
    (define strategy : Symbol (cmdopt-string->symbol option str.name))
    (define level#0 : Positive-Byte (cmdopt-string+>byte option str.#0))
    (define level#n : Positive-Byte (cmdopt-string+>byte option str.#n))

    (list strategy level#0 (min (add1 level#n) 10))))

(define-cmdlet-option lz77-flags #: Lz77-Flags
  #:program 'lz77
  #:args [file.zip]

  #:once-each
  [[(#\B)   #:=> cmdopt-string+>byte hash-Bits #: Positive-Byte "use ~1 as the default hash bits"]
   [(#\m)   #:=> cmdopt-string+>byte min-match #: Positive-Byte "use ~1 as the default minimum match"]
   [(#\F)   #:=> cmdopt-string->index farthest #: Index         "use ~1 as the distance limit for short matches"]
   [(#\f)   #:=> cmdopt-string->index filtered #: Index         "use ~1 as the filtered threshold to throw away random distributed data"]
   [(#\v)   #:=> lz77-verbose                                   "run with verbose messages"]]

  #:multi
  [[(#\s strategy) #:=> lz77-strategy strategy level0 leveln #: (List Symbol Byte Index)
                   "run with the strategy ~1 and compression levels in range [~2, ~3]"]])

(define lz77-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-run : (-> Bytes (Listof ZIP-Strategy) Positive-Byte (Option Positive-Byte) Index Index Void)
  (lambda [txt strategies bits min-match farthest filtered]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))

    (printf "[size: ~a] [hash Bits: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a]~n"
            (~size (bytes-length txt)) bits (or min-match 'auto) farthest filtered)
    
    (define record-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym dest-idx)
         (vector-set! magazine dest-idx sym)]
        [(distance size dest-idx)
         (vector-set! magazine dest-idx (lz77-backref-pair distance size))]))

    (define display-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym dest-idx)
         (record-symbol sym dest-idx)
         (display (integer->char sym))
         (flush-output (current-output-port))]
        [(pointer size dest-idx)
         (record-symbol pointer size dest-idx)
         (display (cons pointer size))
         (flush-output (current-output-port))]))

    (define widths : (Listof Index)
      (text-column-widths (list (list "T" "strategy-name" "00.000KB" "100.00%"
                                      "00.000" "00.000" "00.000" "00.000KB"))))

    (for ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
      (define desc : String (strategy->description strategy))
        
      (when (lz77-verbose) (printf ">>> [strategy: ~a]~n" desc))

      (collect-garbage*)

      (define memory0 : Natural (current-memory-use 'cumulative))
      (define-values (&csize cpu real gc)
        (time-apply (λ [] (lz77-deflate #:hash-bits bits #:min-match min-match #:farthest farthest
                                        txt (if (lz77-verbose) display-symbol record-symbol) strategy))
                    null))
      
      (when (lz77-verbose) (newline))
      
      (define csize : Index (car &csize))
      (define memory : Integer (- (current-memory-use 'cumulative) memory0))
      (define-values (?txt total) (lz77-inflate (in-vector magazine 0 csize)))
      
      (let ([ok? (bytes=? txt ?txt)])
        (when (and (not ok?) (lz77-verbose))
          (echof "==>~n~a~n" ?txt #:fgcolor 'red))

        (define summary : (Listof String)
          (list (if (not ok?) "F" "T")
                desc (zip-size csize) (zip-cfactor csize rsize 2)
                (~gctime cpu) (~gctime real) (~gctime gc) (~size memory)))

        (define color : Term-Color (if (not ok?) 'red 'green))
          
        (for ([col (in-list summary)]
              [wid (in-list widths)]
              [idx (in-naturals)])
          (when (> idx 0) (display #\space))
          
          (let ([numerical? (> idx 1)])
            (echof #:fgcolor color
                   (~a #:align (if numerical? 'right 'left)
                       #:min-width wid
                       col))))

        (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-lz77-flags argument-list #:help-output-port (current-output-port)))
    (define src.txt (λargv))

    (define user-strategies : (Listof ZIP-Strategy)
      (filter zip-strategy?
              (apply append
                     (for/list : (Listof (Listof (Option ZIP-Strategy))) ([s (in-list (lz77-flags-strategy options))])
                       (define levels : (Listof Index) (range (cadr s) (caddr s)))
                       
                       (case (car s)
                         [(normal fast) (map zip-normal-preference levels)]
                         [(default backward) (map zip-backward-preference levels)]
                         [(lazy slow) (map zip-lazy-preference levels)]
                         [(rle run) (map zip-run-preference (filter positive-byte? levels))]
                         [else (list (zip-special-preference (car s)))])))))

    (define fallback-strategies : (Listof ZIP-Strategy)
      (append (map zip-special-preference '(identity plain))
              (map zip-normal-preference (range 1 10))
              (map zip-lazy-preference (range 1 10))
              (map zip-backward-preference (range 1 10))
              (map zip-run-preference (range 1 5))))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (time-apply* (λ [] (let ([tracer (thread (make-zip-log-trace))])
                                 (displayln (if (file-exists? src.txt) (simple-form-path src.txt) src.txt))
                                 
                                 (lz77-run (if (file-exists? src.txt) (file->bytes src.txt) (string->bytes/utf-8 src.txt))
                                           (if (pair? user-strategies) user-strategies fallback-strategies)
                                           (or (lz77-flags-B options) lz77-default-hash-bits)
                                           (lz77-flags-m options)
                                           (or (lz77-flags-F options) lz77-default-farthest)
                                           (or (lz77-flags-f options) 0))
                                 
                                 (dtrace-datum-notice eof)
                                 (thread-wait tracer))))))))

(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (lz77-verbose) 'trace 'info))))

(define strategy->description : (-> ZIP-Strategy String)
  (lambda [s]
    (define name : Symbol (zip-strategy-name s))
    
    (cond [(zip-normal-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-backward-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-lazy-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-run-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [else (symbol->string name)])))

(module+ main
  (main (current-command-line-arguments)))
