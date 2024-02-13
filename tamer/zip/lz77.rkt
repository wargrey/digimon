#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/file)
(require racket/path)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/format)
(require digimon/debug)
(require digimon/echo)

(require digimon/digitama/predicate)
(require digimon/digitama/bintext/lz77)
(require digimon/digitama/bintext/archive)
(require digimon/digitama/bintext/zipconfig)

(require "zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LZ77-Run (-> Bytes (Listof ZIP-Strategy) Positive-Byte Index Index Positive-Byte Positive-Byte Symbol Void))

(define lz77-strategy : (-> Symbol String String String (List Symbol Byte Index))
  (lambda [option str.name str.#0 str.#n]
    (define strategy : Symbol (cmdopt-string->symbol option str.name))
    (define level#0 : Positive-Byte (cmdopt-string+>byte option str.#0))
    (define level#n : Positive-Byte (cmdopt-string+>byte option str.#n))

    (list strategy level#0 (min (add1 level#n) 10))))

(define-cmdlet-option lz77-flags #: Lz77-Flags
  #:program 'lz77
  #:args [filename]

  #:once-each
  [[(#\m)   #:=> cmdopt-string+>byte min-match   #: Positive-Byte "use ~1 as the default minimum match"]
   [(#\F)   #:=> cmdopt-string->index farthest   #: Index         "use ~1 as the distance limit for short matches"]
   [(#\f)   #:=> cmdopt-string->index filtered   #: Index         "use ~1 as the filtered threshold to throw away random distributed data"]
   
   [(#\W)   #:=> cmdopt-string+>byte winbits     #: Positive-Byte "encoding with the slide window of ~1 bits"]
   [(#\M)   #:=> cmdopt-string+>byte memlevel    #: Positive-Byte "encoding in memory level of ~1"]

   [(#\c)   #:=> cmdopt-string->symbol huffcodes #: Symbol        "specific the huffman codes"]

   [(#\b)   #:=> lz77-brief                                       "only display T or F as the test result of a test"]
   [(#\v)   #:=> lz77-verbose                                     "run with verbose messages"]
   [(#\h)   #:=> lz77-hexdump                                     "dump the hexdecicaml codes of the content"]]

  #:multi
  [[(#\s strategy) #:=> lz77-strategy strategy level0 leveln #: (List Symbol Byte Index)
                   "run with the strategy ~1 and compression levels in range [~2, ~3]"]])

(define lz77-brief : (Parameterof Boolean) (make-parameter #false))
(define lz77-verbose : (Parameterof Boolean) (make-parameter #false))
(define lz77-hexdump : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-run : LZ77-Run
  (lambda [txt strategies min-match farthest filtered winbits memlevel huffcodes]
    (define rsize : Index (bytes-length txt))
    (define magazine : (Vectorof LZ77-Symbol) (make-vector rsize 0))
    (define mgz-idx : Natural 0)

    (printf "[window size: ~a] [memory level: ~a] [minimum match: ~a] [farthest: ~a] [filtered: ~a]~n"
            (~size (arithmetic-shift 1 winbits)) memlevel (or min-match 'auto) farthest filtered)
    
    (define record-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym final?)
         (vector-set! magazine mgz-idx sym)
         (set! mgz-idx (+ mgz-idx 1))]
        [(distance size final?)
         (vector-set! magazine mgz-idx (lz77-backref-pair distance size))
         (set! mgz-idx (+ mgz-idx 1))]))

    (define display-symbol : LZ77-Submit-Symbol
      (case-lambda
        [(sym final?)
         (record-symbol sym final?)
         (display (integer->char sym))
         (flush-output (current-output-port))]
        [(pointer size final?)
         (record-symbol pointer size final?)
         (display (cons pointer size))
         (flush-output (current-output-port))]))

    (define widths : (Listof Index)
      (text-column-widths (list (list "T" "strategy-name" "000.000KB" "100.00%"
                                      "00.000" "00.000" "00.000" "000.000KB"))))

    (for ([strategy (if (list? strategies) (in-list strategies) (in-value strategies))])
      (define desc : String (strategy->description strategy))
      
      (when (lz77-verbose) (printf ">>> [strategy: ~a]~n" desc))

      (define-values (csize memory cpu real gc)
        (time-apply** (λ [] (set! mgz-idx 0)
                        (lz77-deflate* #:window-bits winbits #:memory-level memlevel #:min-match min-match #:farthest farthest
                                       txt (if (lz77-verbose) display-symbol record-symbol) strategy)
                        (assert mgz-idx index?))))
      
      (when (lz77-verbose) (newline))
      (define-values (?txt total) (lz77-inflate (in-vector magazine 0 csize)))
      
      (lz77-display-summary desc txt ?txt csize rsize widths memory cpu real gc))

    (when (and (not (lz77-verbose)) (lz77-brief))
      (newline))))

(define lz77-display-summary : (-> String Bytes Bytes Natural Index (Listof Index) Integer Natural Natural Natural Void)
  (lambda [desc txt ?txt csize rsize widths memory cpu real gc]
    (let ([ok? (bytes=? txt ?txt)])
      (define summary : (Listof String)
        (list (if (not ok?) "F" "T")
              desc (zip-size csize) (zip-cfactor csize rsize 2)
              (~gctime cpu) (~gctime real) (~gctime gc) (~size memory)))
      
        (define color : Term-Color (if (not ok?) 'red 'green))

      (cond [(and (not (lz77-verbose)) (lz77-brief))
             (echof "~a " (car summary) #:fgcolor color)
             (flush-output (current-output-port))]
            [else
             (for ([col (in-list summary)]
                   [wid (in-list widths)]
                   [idx (in-naturals)])
               (when (> idx 0) (display #\space))
               
               (let ([numerical? (> idx 1)])
                 (echof #:fgcolor color
                        (~a #:align (if numerical? 'right 'left)
                            #:min-width wid
                            col))))
             (newline)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lz77-run-on-text : (-> String Lz77-Flags (Listof ZIP-Strategy) LZ77-Run Void)
  (lambda [src.txt options strategies run]
    (if (file-exists? src.txt)
        (printf "~a [~a]~n" (simple-form-path src.txt) (~size (file-size src.txt)))
        (printf "~a [~a]~n" src.txt (~size (string-utf-8-length src.txt))))

    (let ([txt (if (file-exists? src.txt) (file->bytes src.txt) (string->bytes/utf-8 src.txt))])
      (when (lz77-verbose) (displayln txt))
      
      (run txt strategies
           (or (lz77-flags-m options) lz77-default-min-match)
           (or (lz77-flags-F options) lz77-default-farthest)
           (or (lz77-flags-f options) 0)
           (or (lz77-flags-W options) 12)
           (or (lz77-flags-M options) 1)
           (or (lz77-flags-c options) 'auto)))))

(define lz77-main : (->* ((U (Listof String) (Vectorof String))) (LZ77-Run) Nothing)
  (lambda [argument-list [run lz77-run]]
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
                         [(rle run) (map zip-run-preference (filter positive-index? levels))]
                         [else (list (zip-special-preference (car s)))])))))

    (define fallback-strategies : (Listof ZIP-Strategy)
      (append (map zip-special-preference '(identity plain))
              (map zip-normal-preference (range 1 10))
              (map zip-lazy-preference (range 1 10))
              (map zip-backward-preference (range 1 10))
              (map zip-run-preference (range 1 5))))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (define strategies : (Listof ZIP-Strategy)
                (if (pair? user-strategies) user-strategies fallback-strategies))
              
              (cond [(directory-exists? src.txt)
                     (let ([entries (make-archive-directory-entries src.txt #:configure defualt-archive-ignored-configure+compiled #:keep-directory? #false)])
                       (let run-on-dir ([es : Archive-Entries entries])
                         (for ([e (in-list es)])
                           (cond [(list? e) (run-on-dir e)]
                                 [else (let ([src (~a (archive-entry-source e))])
                                         (lz77-run-on-text src options strategies run))]))))]
                    [else (lz77-run-on-text src.txt options strategies run)])
              
              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (lz77-verbose) 'trace 'info))))

(define strategy->description : (-> ZIP-Strategy String)
  (lambda [s]
    (define name : Symbol (zip-strategy-name s))
    
    (cond [(zip-normal-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-backward-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-lazy-strategy? s) (format "~a:~a" name (zip-strategy-level s))]
          [(zip-run-strategy? s) (format "~a:~a" name (zip-run-strategy-length s))]
          [else (symbol->string name)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (lz77-main (current-command-line-arguments)))
