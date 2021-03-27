#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require "zipinfo.rkt")
(require "zipconfig.rkt")
(require "deflation.rkt")
(require "archive.rkt")

(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../format.rkt")
(require "../../checksum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PKZIP-Strategy (U (Pairof Symbol Index) Index Symbol ZIP-Strategy False))

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
(define zip-write-entry : (-> Output-Port Archive-Entry (Option Path-String) (Option Path-String) Regexp
                              Boolean PKZIP-Strategy Positive-Byte Bytes
                              (Option ZIP-Directory))
  (lambda [/dev/zipout entry root zip-root px:suffix seekable? ?strategy memory-level pool]
    (define entry-source : (U Bytes Path) (archive-entry-source entry))
    (define regular-file? : Boolean (or (bytes? entry-source) (file-exists? entry-source)))
    (define entry-name : String (zip-path-normalize (archive-entry-reroot (archive-entry-name entry) root zip-root 'stdin) regular-file?))
    (define-values (mdate mtime) (zip-entry-modify-datetime (or (archive-entry-utc-time entry) (current-seconds))))

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
      (or (for/or : (Option ZIP-Strategy) ([opt (in-list (cons ?strategy (archive-entry-options entry)))])
            (cond [(zip-strategy? opt) opt]
                  [(index? opt) (zip-level->maybe-strategy opt)]
                  [(symbol? opt) (zip-name->maybe-strategy opt)]
                  [(and (pair? opt) (symbol? (car opt)) (index? (cdr opt)))
                   (zip-name->maybe-strategy (car opt) (cdr opt))]
                  [else #false]))
          (zip-default-preference)))

    (define fixed-only? : Boolean (and (memq 'fixed (archive-entry-options entry)) #true))

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
             (let ([desc (zip-write-entry-body pool /dev/zipout entry-source entry-name method strategy memory-level fixed-only?)])
               (write-zip-data-descriptor desc /dev/zipout)
               desc)]
            [else
             (let* ([self-size (sizeof-zip-entry self-local)]
                    [desc (zip-write-entry-body pool /dev/zipout entry-source entry-name method strategy memory-level fixed-only? (+ position self-size))])
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
                        #:filename entry-name #:relative-offset (assert position index?)
                        #:crc32 (zip-data-descriptor-crc32 sizes) #:csize (zip-data-descriptor-csize sizes) #:rsize (zip-data-descriptor-rsize sizes)
                        #:gpflag flag #:compression method #:mdate mdate #:mtime mtime
                        #:internal-attributes (if (archive-entry-ascii? entry) #b1 #b0)
                        #:external-attributes (zip-permission-attribute (archive-entry-permission entry) (not regular-file?))
                        #:comment (or (archive-entry-comment entry) ""))))

(define zip-write-entry-body : (->* (Bytes Output-Port (U Bytes Path) String ZIP-Compression-Method ZIP-Strategy Positive-Byte Boolean)
                                    ((Option Natural))
                                    ZIP-Data-Descriptor)
  (lambda [pool /dev/stdout source entry-name method strategy memory-level static-only? [seek #false]]
    (define anchor : Natural
      (cond [(not seek) (file-position /dev/stdout)]
            [else (file-position /dev/stdout seek) seek]))

    (define /dev/zipout : Output-Port
      (case method
        [(deflated)
         (open-output-deflated-block /dev/stdout strategy #false #:fixed-only? static-only? #:memory-level memory-level #:name entry-name)]
        [else /dev/stdout]))
    
    (define-values (rsize crc32)
      (if (bytes? source)
          (let ([rsize : Natural (bytes-length source)])
            (when (> rsize 0)
              (write-bytes source /dev/zipout))
            (values rsize (checksum-crc32 source 0 rsize)))

          (zip-entry-copy (open-input-file source) /dev/zipout pool

                          ; some systems may limit the maximum number of open files
                          ; if exception escapes, constructed ports would be closed by the custodian
                          #:close-input-port? #true

                          ; keep consistent with other kind of sources
                          #:flush-output-port? #false)))

    ; by design, all output ports associated with compression methods should follow the convention:
    ;   never mark the block with the FINAL flag when flushing manually,
    ;   and let the closing flush and terminate the bitstream.
    ; as such, only do flushing on non-compression output port,
    ;   and we would have a chance to save some bytes for another empty final block
    ;   if the bitstream isn't block aligned.
    (if (eq? /dev/stdout /dev/zipout)
        (flush-output /dev/zipout)
        (close-output-port /dev/zipout))
    
    (let ([csize (- (file-position /dev/stdout) anchor)])
      (make-zip-data-descriptor #:crc32 (assert crc32 index?)
                                #:csize (assert csize index?)
                                #:rsize (assert rsize index?)))))

(define zip-write-directories : (-> Output-Port (Option String) (Rec zds (Listof (U ZIP-Directory False zds))) Void)
  (lambda [/dev/zipout comment cdirs]
    (define offset : Natural (file-position /dev/zipout))
    
    (define-values (cdirsize count)
      (let write-directories : (Values Natural Natural)
        ([cdirs : (Rec zds (Listof (U ZIP-Directory False zds))) (reverse cdirs)]
         [size0 : Natural 0]
         [count0 : Natural 0])
        (for/fold ([size : Natural size0] [count : Natural count0])
                  ([cdir (in-list cdirs)] #:when cdir)
          (cond [(list? cdir) (write-directories (reverse cdir) size count)]
                [else (values (+ size (write-zip-directory cdir /dev/zipout)) (+ count 1))]))))

    #;(assert (- (file-position /dev/zipout) offset cdirsize) zero?)

    (define eocdr : zip-end-of-central-directory
      (with-asserts ([offset index?]
                     [cdirsize index?]
                     [count index?])
        (make-zip-end-of-central-directory #:cdir-offset offset #:cdir-size cdirsize
                                           #:entry-count count #:entry-total count
                                           #:comment (or comment ""))))

    (write-zip-end-of-central-directory eocdr /dev/zipout)
    (flush-output /dev/zipout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-folder-entry? : (-> ZIP-Directory Boolean)
  (lambda [cdir]
    (let ([fname (zip-directory-filename cdir)])
      (eq? (string-ref fname (sub1 (string-length fname))) #\/))))

(define open-input-zip-entry : (-> Input-Port ZIP-Directory #:verify? Boolean Input-Port)
  (lambda [/dev/zipin cdir #:verify? verify?]
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
(define zip-entry-copy : (->* (Input-Port Output-Port) ((U Bytes Index False) #:close-input-port? Boolean #:flush-output-port? Boolean) (Values Natural Index))
  (lambda [/dev/zipin /dev/zipout [pool0 4096] #:close-input-port? [close-in? #false] #:flush-output-port? [flush-out? #true]]
    (define-values (#{pool : Bytes} #{pool-size : Index})
      (cond [(bytes? pool0) (values pool0 (bytes-length pool0))]
            [(exact-positive-integer? pool0) (values (make-bytes pool0 0) pool0)]
            [else (values (make-bytes 4096 0) 4096)]))

    (let copy-entry/checksum ([total : Natural 0]
                              [crc32 : Index 0])
      (define read-size : (U EOF Nonnegative-Integer Procedure) (read-bytes-avail! pool /dev/zipin 0 pool-size))
      
      (cond [(exact-positive-integer? read-size)
             (write-bytes pool /dev/zipout 0 read-size)
             (copy-entry/checksum (+ total read-size) (checksum-crc32* pool crc32 0 read-size))]
            [(eof-object? read-size)
             (when (and flush-out?) (flush-output /dev/zipout))
             (when (and close-in?) (close-input-port /dev/zipin))
             (values total crc32)]
            [else ; deadcode. skip special values
             (copy-entry/checksum total crc32)]))))

(define zip-entry-copy/trap : (->* (Input-Port Output-Port Natural Index) ((U Bytes Index False)) (U String True))
  (lambda [/dev/zipin /dev/zipout rSize CRC32 [pool0 4096]]
    (with-handlers ([exn:fail? exn-message])
      (let-values ([(rsize crc32) (zip-entry-copy /dev/zipin /dev/zipout pool0)])
        (cond [(not (= rSize rsize)) (format "bad size ~a (should be ~a)" rsize rSize)]
              [(not (= CRC32 crc32)) (format "bad checksum ~a (should be ~a)" (~hexstring crc32) (~hexstring CRC32))]
              [else #true])))))

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
