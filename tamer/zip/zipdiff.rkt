#lang typed/racket/gui

(provide (all-defined-out))

(require digimon/port)
(require digimon/stdio)
(require digimon/thread)
(require digimon/format)

(require digimon/cmdopt)

(require digimon/digitama/bintext/zip)
(require digimon/digitama/bintext/deflation)
(require digimon/digitama/bintext/archive/progress)

(require (except-in "zipinfo.rkt" main))
(require (except-in "zip.rkt" main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zipdiff-flags #: Zipdiff-Flags
  #:program 'zipdiff
  #:args [source]

  #:once-each
  [[(#\s strategy)         #:=> zip-strategy#level strategy level #: (Pairof Symbol Byte)
                           "run with the strategy ~1 and compression level ~2"]
   [(#\M)                  #:=> cmdopt-string+>byte memlevel  #: Positive-Byte "encoding in memory level of ~1"]
   [(#\n)                  #:=> cmdopt-string+>index span  #: Positive-Index "test with only first ~1 bytes"]
   [(#\o)                  #:=> cmdopt-string->index skip  #: Index "skip ~1 bytes before testing"]
   [(#\v)                  #:=> zip-verbose
                           "run with verbose messages"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Zipdiff-Text%
  (Class #:implements/inits Text%))

(define-type Zipdiff-Panel%
  (Class #:implements/inits Vertical-Panel%
         (init [options Zipdiff-Flags]
               [source Path]
               [huffman-codes Symbol])))

(define-type Zipdiff%
  (Class #:implements Frame%
         (init [argument-list (U (Listof String) (Vectorof String))])))

(define zipdiff-text% : Zipdiff-Text%
  (class text% (super-new)
    (send this change-style
          (make-object style-delta% 'change-family 'modern))))

(define zipdiff-panel% : Zipdiff-Panel%
  (class vertical-panel% (super-new)
    (init options source huffman-codes)

    (define master : Custodian (make-custodian))
    (define-values (/dev/zipin /dev/zipout) (make-pipe))
    (define-values (/dev/grawin /dev/grawout) (make-pipe))
    (define-values (/dev/gzipin /dev/gzipout) (make-pipe))

    (define pathname : (Instance Message%) (new message% [label (format "~a [~a]" source huffman-codes)] [parent this]))
    (define de-gauge : (Instance Gauge%) (new gauge% [label "Deflating [000.0%]"] [parent this] [range 100]))
    (define zip-zone : (Instance Horizontal-Pane%) (new horizontal-pane% [parent this]))

    (define raw-text : (Instance Zipdiff-Text%) (new zipdiff-text%))
    (define zip-text : (Instance Zipdiff-Text%) (new zipdiff-text%))
    
    (define raw-canvas : (Instance Editor-Canvas%) (new editor-canvas% [parent zip-zone] [editor raw-text]))
    (define zip-canvas : (Instance Editor-Canvas%) (new editor-canvas% [parent zip-zone] [editor zip-text]))

    (define/override (on-superwindow-show shown?)
      (if (or shown?)
          (when (null? (unbox &ghostcats))
            (set-box! &ghostcats
                      (list (thread zipdiff-read)
                            (thread zipdiff-write)
                            (thread zipdiff-display))))
          (zipdiff-stop)))

    (define (zipdiff-read) : Void
      (with-handlers ([exn:break? void])
        (parameterize ([current-custodian master])
          (define pool : Bytes (make-bytes 4096))
          (define /dev/zhexout : Output-Port (open-output-hexdump /dev/gzipout #true #:upcase? #true #:ascii? #false))
          (define /dev/stdin : Input-Port (open-input-deflated-block /dev/zipin #false #true #:name #false))
          
          (zip-entry-copy /dev/stdin /dev/zhexout pool)
          (close-output-port /dev/zhexout))))

    (define (zipdiff-write) : Void
      (with-handlers ([exn:break? void])
        (parameterize ([current-custodian master])
          (define pool : Bytes (make-bytes 4096))
          (define-values (strategy huffcodes) (zip-strategy-normalize (zipdiff-flags-strategy options) (list huffman-codes)))
          (define /dev/rhexout : Output-Port (open-output-hexdump /dev/grawout #true #:upcase? #true #:ascii? #false))
          (define /dev/stdin : (U Path Bytes)
            (let ([n (or (zipdiff-flags-n options) 0)]
                  [o (or (zipdiff-flags-o options) 0)])
              (cond [(= o n 0) source]
                    [else (call-with-input-file source
                            (λ [[/dev/stdin : Input-Port]] : Bytes
                              (when (> o 0) (drop-bytes /dev/stdin o))
                              (let ([head (read-bytes n /dev/stdin)])
                                (cond [(eof-object? head) #""]
                                      [else head]))))])))
          
          (parameterize ([default-archive-entry-progress-handler (zipdiff-make-gauge de-gauge "Deflating")])
            (zip-write-entry-body pool /dev/zipout /dev/stdin (path->string source)
                                  (if (bytes? /dev/stdin) (bytes-length /dev/stdin) (file-size source))
                                  'deflated strategy (or (zipdiff-flags-M options) 8) huffcodes
                                  ((default-archive-progress-topic-resolver) /dev/zipout))
            
            (close-output-port /dev/zipout)
            (close-output-port /dev/rhexout)))))

    (define zipdiff-display : (case-> [String (Instance Zipdiff-Text%) -> Void]
                                      [-> Void])
      (case-lambda
        [(hexline t)
         (send t begin-edit-sequence)
         (send t insert hexline)
         (send t insert #\newline)
         (send t end-edit-sequence)]
        [()
         (with-handlers ([exn:break? void])
           (let sync-read-display-loop : Void ()
             (sync/enable-break /dev/gzipin)

             (let* ([rawline (read-line /dev/grawin)]
                    [hexline (read-line /dev/gzipin)])
               (when (and (string? rawline) (string? hexline))
                 (zipdiff-display rawline raw-text)
                 
                 (if (string-ci=? rawline hexline)
                     (zipdiff-display hexline zip-text)
                     (zipdiff-display (string-append hexline " F") zip-text))
                 
                 (sync-read-display-loop)))))]))

    (define (zipdiff-stop) : Void
      (thread-safe-kill (unbox &ghostcats))
      (custodian-shutdown-all master))

    (define (zipdiff-make-gauge [gauge : (Instance Gauge%)] [label : String]) : Archive-Entry-Progress-Handler
      (lambda [topic entry current total done?]
        (define % (if (>= current total) 1 (/ current total)))

        (send gauge set-label (format "~a [~a]" label (~% % #:precision '(= 1))))
        (send gauge set-value (exact-round (* 100 %)))))

    (define &ghostcats : (Boxof (Listof Thread)) (box null))))

(define zipdiff% : Zipdiff%
  (class frame% (init argument-list)
    (super-new [label "Deflate Diff"] [parent #false] [enabled #true]
               [style '(fullscreen-button)] [stretchable-width #true] [stretchable-height #true]
               [x 0] [y 0] [width #false] [height #false] [min-width 1200] [min-height 800]
               [alignment '(center top)] [border 0] [spacing 0])

    (define zipdiffs : (Listof (Instance Zipdiff-Panel%))
      (let*-values ([(opts λsrc) (parse-zipdiff-flags argument-list #:help-output-port (current-output-port))]
                    [(srcfile) (simple-form-path (λsrc))]
                    [(pstyle) '(border auto-vscroll)])
        (list (new zipdiff-panel% [parent this] [options opts] [source srcfile] [huffman-codes 'fixed] [style pstyle])
              (new zipdiff-panel% [parent this] [options opts] [source srcfile] [huffman-codes 'dynamic] [style pstyle])
              (new zipdiff-panel% [parent this] [options opts] [source srcfile] [huffman-codes 'auto] [style pstyle]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define frame (new zipdiff% [argument-list (current-command-line-arguments)]))
  
  (send frame show #true)
  (send frame maximize #true))
