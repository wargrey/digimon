#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require "zipinfo.rkt")
(require "zipconfig.rkt")
(require "deflate.rkt")
(require "archive.rkt")

(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../date.rkt")
(require "../../checksum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PKZIP-Strategy (U Byte Symbol False))

(define pkzip-compression-methods : (Listof ZIP-Compression-Method) (list 'stored 'deflated))
(define pkzip-digimon-version : Byte 62)
(define pkzip-extract-system : ZIP-System 'FAT)
(define pkzip-extract-version : Byte 20)

(define pkzip-host-system : ZIP-System
  (case (system-type 'os)
    [(macosx) 'Darwin]
    [(unix) 'UNIX]
    [else 'NTFS]))

(define pkzip-default-suffixes : (Listof Symbol) (list 'zip 'Z 'zoo 'arc 'lzh 'arj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-write-entry : (-> Output-Port Archive-Entry (Option Path-String) (Option Path-String) Regexp Boolean PKZIP-Strategy (Option ZIP-Directory))
  (lambda [/dev/zipout entry root zip-root px:suffix seekable? ?strategy]
    (define entry-source : (U Bytes Path) (archive-entry-src entry))
    (define regular-file? : Boolean (or (bytes? entry-source) (file-exists? entry-source)))
    (define entry-name : String (archive-entry-reroot (zip-path-normalize (archive-entry-name entry) regular-file?) root zip-root 'stdin))
    (define-values (mdate mtime) (utc-seconds->msdos-datetime (or (archive-entry-utc-time entry) (current-seconds)) #true))

    (define method : ZIP-Compression-Method
      (cond [(not regular-file?) 'stored]
            [else (let check-method ([ms : (Listof Symbol) (archive-entry-methods entry)])
                    (cond [(pair? ms)
                           (let ([candidates (memq (car ms) pkzip-compression-methods)])
                             (cond [(not candidates) (check-method (cdr ms))]
                                   [else (car candidates)]))]
                          [(regexp-match? px:suffix entry-name) 'stored]
                          [else 'deflated]))]))

    (define strategy : ZIP-Strategy
      (or (and ?strategy
               (or (and (zip-compression-level? ?strategy)
                        (zip-default-preference ?strategy)))
               (or (and (symbol? ?strategy)
                        (zip-name->maybe-strategy ?strategy))))
          (for/or : (Option ZIP-Strategy) ([opt (in-list (archive-entry-options entry))])
            (and (symbol? opt) (zip-name->maybe-strategy opt)))
          (zip-name->maybe-strategy 'default)))

    (define fixed-only? : Boolean (and (memq 'fixed (archive-entry-options entry)) #true))

    (define memory-level : Positive-Byte
      (let ([levels (filter (λ [[o : Any]] (and (byte? o) (< 0 o) (<= o 8))) (archive-entry-options entry))])
        (cond [(pair? levels) (car levels)]
              [else 8])))

    (define flag : Index
      (case method
        [(deflated) (zip-deflation-flag (zip-strategy-flag strategy) (not seekable?))]
        [else 0]))

    (define position : Natural (file-position /dev/zipout))
    (define self-local : ZIP-Entry
      (make-zip-entry #:extract-system pkzip-extract-system #:extract-version pkzip-extract-version
                      #:filename entry-name #:gpflag flag #:compression method #:mdate mdate #:mtime mtime))

    (define sizes : ZIP-Data-Descriptor
      (cond [(not regular-file?)
             (write-zip-entry self-local /dev/zipout)
             (make-zip-data-descriptor #:crc32 0 #:csize 0 #:rsize 0)]
            [(not seekable?)
             (write-zip-entry self-local /dev/zipout)
             (let ([desc (zip-write-entry-body /dev/zipout entry-source entry-name method strategy memory-level (not fixed-only?))])
               (write-zip-data-descriptor desc /dev/zipout)
               desc)]
            [else
             (let* ([self-size (sizeof-zip-entry self-local)]
                    [desc (zip-write-entry-body /dev/zipout entry-source entry-name method strategy memory-level (not fixed-only?) (+ position self-size))])
               (file-position /dev/zipout position)
               (write-zip-entry (remake-zip-entry self-local
                                                  #:crc32 (zip-data-descriptor-crc32 desc)
                                                  #:csize (zip-data-descriptor-csize desc)
                                                  #:rsize (zip-data-descriptor-rsize desc))
                                /dev/zipout)
               (file-position /dev/zipout (+ position self-size (zip-data-descriptor-csize desc)))
               desc)]))

    (make-zip-directory #:create-system pkzip-host-system #:create-version pkzip-digimon-version
                        #:extract-system pkzip-extract-system #:extract-version pkzip-extract-version
                        #:filename (zip-entry-filename self-local) #:relative-offset (assert position index?)
                        #:crc32 (zip-data-descriptor-crc32 sizes) #:csize (zip-data-descriptor-csize sizes) #:rsize (zip-data-descriptor-rsize sizes)
                        #:gpflag flag #:compression method #:mdate mdate #:mtime mtime
                        #:internal-attributes (if (archive-entry-ascii? entry) #b1 #b0)
                        #:external-attributes (zip-permission-attribute (archive-entry-permission entry) (not regular-file?))
                        #:comment (or (archive-entry-comment entry) ""))))

(define zip-write-entry-body : (->* (Output-Port (U Bytes Path) String ZIP-Compression-Method ZIP-Strategy Positive-Byte Boolean)
                                    ((Option Natural))
                                    ZIP-Data-Descriptor)
  (let* ([pool-size : Index 4096]
         [pool : Bytes (make-bytes pool-size)])
    (lambda [/dev/stdout source entry-name method strategy memory-level allow-dynamic-block? [seek #false]]
      (when (exact-integer? seek)
        (file-position /dev/stdout seek))

      (define /dev/zipin : (U Input-Port Bytes)
        (cond [(bytes? source) source]
              [else (open-input-file source)]))

      (define /dev/zipout : Output-Port
        (case method
          ; Just leave all constructed ports to the custodian so that the original output port won't be closed unexpectedly.
          [(deflated)
           (open-output-deflated-block #:dynamic-block? allow-dynamic-block? #:memory-level memory-level
                                       #:name entry-name #:safe-flush-on-close? #false
                                       /dev/stdout strategy)]
          [else /dev/stdout #| the original one that shouldn't be closed here |#]))

      (define-values (crc32 rsize)
        (if (bytes? /dev/zipin)
            (let ([rsize : Natural (bytes-length /dev/zipin)])
              (when (> rsize 0)
                (write-bytes /dev/zipin /dev/zipout))
              (flush-output /dev/zipout)
              (values (checksum-crc32 /dev/zipin 0 rsize) rsize))
            (let store : (Values Index Natural) ([crc32 : Index 0]
                                                 [rsize : Natural 0])
              (define read-size : (U EOF Positive-Integer) (read-bytes! pool /dev/zipin 0 pool-size))
              (cond [(eof-object? read-size) (flush-output /dev/zipout) (values crc32 rsize)]
                    [else (let ([size++ (+ rsize read-size)]
                                [crc++ (checksum-crc32* pool crc32 0 read-size)])
                            (write-bytes pool /dev/zipout 0 read-size)
                            (store crc++ size++))]))))

      (define csize : Natural
        (cond [(eq? /dev/zipout /dev/stdout) rsize]
              [else (file-position /dev/zipout)]))

      (make-zip-data-descriptor #:crc32 (assert crc32 index?)
                                #:csize (assert csize index?)
                                #:rsize (assert rsize index?)))))

(define zip-write-directories : (-> Output-Port (Option String) (Listof (Option ZIP-Directory)) Natural)
  (lambda [/dev/zipout comment cdirs]
    (define offset : Natural (file-position /dev/zipout))
    (define count : Index (length cdirs))
    (define cdirsize : Natural
      (for/fold ([size : Natural 0])
                ([cdir (in-list cdirs)] #:when cdir)
        (+ size (write-zip-directory cdir /dev/zipout))))
    (define eocdr : zip-end-of-central-directory
      (make-zip-end-of-central-directory #:cdir-offset (assert offset index?) #:cdir-size (assert cdirsize index?)
                                         #:entry-count count #:entry-total count
                                         #:comment (or comment "")))
    (+ cdirsize (write-zip-end-of-central-directory eocdr /dev/zipout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-folder-entry? : (-> ZIP-Directory Boolean)
  (lambda [cdir]
    (let ([fname (zip-directory-filename cdir)])
      (eq? (string-ref fname (sub1 (string-length fname))) #\/))))

(define open-input-zip-entry : (-> Input-Port ZIP-Directory Input-Port)
  (lambda [/dev/zipin cdir]
    (define ?zip (regexp-match #px"[^.]+$" (archive-port-name /dev/zipin)))
    (define port-name (format "~a://~a" (if (not ?zip) 'zip (car ?zip)) (zip-directory-filename cdir)))

    (file-position /dev/zipin (zip-directory-relative-offset cdir))
    (read-zip-entry /dev/zipin) ; for the sake of efficiency, no further validity check for entries here.

    (when (zip-encrypted? (zip-directory-gpflag cdir))
      (throw-unsupported-error /dev/zipin 'open-input-zip-entry "encryped entry: ~a" (zip-directory-filename cdir)))
    
    (case (zip-directory-compression cdir)
      [(deflated) (open-input-deflated-block /dev/zipin (zip-directory-csize cdir) #false #:name port-name #:error-name 'zip:deflated)]
      [else (open-input-block /dev/zipin (zip-directory-csize cdir) #false #:name port-name)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-path-normalize : (case-> [Path-String -> String]
                                     [Path-String Boolean -> String])
  (let ([pxwin-separator #rx"\\\\"])
    (case-lambda
      [(filename)
       (cond [(string? filename) (regexp-replace* pxwin-separator filename "/")]
             [else (case (path-convention-type filename)
                     [(windows) (regexp-replace* pxwin-separator (path->string filename) "/")]
                     [else (path->string filename)])])]
      [(filename regular-file?)
       (cond [(not regular-file?) (zip-path-normalize (path->directory-path filename))]
             [(string? filename) (zip-path-normalize (string-trim filename #rx"(/|\\\\)$" #:left? #false #:right? #true #:repeat? #false))]
             [else (zip-path-normalize (path->string filename) regular-file?)])])))
