#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require "zipinfo.rkt")
(require "zipconfig.rkt")
(require "deflation.rkt")
(require "archive.rkt")

(require "archive/progress.rkt")

(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../stdio.rkt")
(require "../../format.rkt")
(require "../../checksum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PKZIP-Strategy (U (Pairof Symbol Index) Index Symbol ZIP-Strategy False))

(define pkzip-compression-methods : (Listof ZIP-Compression-Method) (list 'stored 'deflated))
(define pkzip-digimon-version : Byte 62)
(define pkzip-extraction-system : ZIP-System 'FAT)
(define pkzip-extraction-version : Byte 20)
(define pkzip-extraction-version64 : Byte 45)

(define pkzip-host-system : ZIP-System
  (case (system-type 'os)
    [(macosx) 'Darwin]
    [(unix) 'UNIX]
    [else 'NTFS]))

(define pkzip-default-suffixes : (Listof Symbol) (list 'zip 'Z 'zoo 'arc 'lzh 'arj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-write-entry : (-> Output-Port Archive-Entry (Option Path-String) (Option Path-String) Regexp
                              Boolean PKZIP-Strategy Positive-Byte Boolean Bytes
                              (Option ZIP-Directory))
  (lambda [/dev/zipout entry root zip-root px:suffix seekable? ?strategy memlevel force-zip64? pool]
    (define entry-source : (U Bytes Path) (archive-entry-source entry))
    (define regular-file? : Boolean (archive-entry-regular-file? entry))
    (define entry-name : String (zip-path-normalize (archive-entry-reroot (archive-entry-get-name entry) root zip-root 'stdin) regular-file?))
    (define topic : Symbol ((default-archive-progress-topic-resolver) /dev/zipout))
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

    (define utf8? : Boolean
      ; WARNING: Extended Language Encoding Data (#x0008) remains undefined
      (let ([LC-ALL (default-stdout-locale)])
        (cond [(or (not LC-ALL) (eq? LC-ALL 'utf-8) (eq? LC-ALL 'UTF-8)) #true]
              [(eq? LC-ALL 'locale) (string-ci=? "UTF-8" (locale-string-encoding))]
              [(string? LC-ALL) (string-ci=? "UTF-8" LC-ALL)]
              [else (string-ci=? "UTF-8" (symbol->immutable-string LC-ALL))])))

    (define-values (strategy huffcodes) (zip-strategy-normalize ?strategy (archive-entry-options entry)))
    (define comment : String (or (archive-entry-comment entry) ""))
    (define has-data-descriptor? : Boolean (not seekable?))
    
    (define flag : Index
      (if (eq? method 'deflated)
          (zip-deflation-flag #:encrypted? #false #:strong-encryption? #false #:encrypt-cdir? #false
                              #:patched-data? #false #:utf8? utf8?
                              (zip-strategy-flag strategy) has-data-descriptor?)
          (zip-stored-flag #:encrypted? #false #:strong-encryption? #false #:encrypt-cdir? #false
                           #:patched-data? #false #:utf8? utf8?
                           has-data-descriptor?)))

    (define position : Natural (file-position /dev/zipout))
    (define rsize : Natural (archive-entry-size entry))
    (define zip64-format? : Boolean (or force-zip64? (>= position 0xFF32) (not (index? rsize #| just in case `csize` will be greater then `rsize` |#))))
    (define /dev/zmiout : Output-Port (open-output-bytes '/dev/zmiout))

    ; please keep the zip64 extended info as the first extra fields, so that we can easily modify the compressed size
    ; TODO: squeeze out every single byte based on the trick of zip64 extended data
    (when (or zip64-format?)
      (write-zipinfo-zip64 (make-zipinfo-zip64 #:rsize rsize #:csize 0 #:relative-offset position) /dev/zmiout))

    (when (not utf8?)
      (let ([name (string->bytes/utf-8 entry-name)])
        (write-zipinfo-unicode-path (make-zipinfo-unicode-path #:crc32 (checksum-crc32 name) #:name name) /dev/zmiout))
      
      (when (> (string-length comment) 0)
        (let ([content (string->bytes/utf-8 comment)])
          (write-zipinfo-unicode-comment (make-zipinfo-unicode-comment #:crc32 (checksum-crc32 content) #:content content) /dev/zmiout))))

    (define extraction-version : Byte (if (not zip64-format?) pkzip-extraction-version pkzip-extraction-version64))
    
    (define self-local : ZIP-File
      (make-zip-file #:esystem pkzip-extraction-system #:eversion extraction-version
                     #:name entry-name #:gpflag flag #:compression method #:mdate mdate #:mtime mtime
                     #:metainfo (get-output-bytes /dev/zmiout #false)))

    ; at this point, the `crc32`, `rsize`, and `csize` are all 0s
    ; which case works for folders and non-seekable output ports
    (write-zip-file self-local /dev/zipout)

    (define-values (#{crc32 : Index} #{csize : Natural})
      (cond [(not regular-file?) (values 0 0) #| blank files should not be skipped here, they might be filled with an empty compressed block |#]
            [else (let-values ([(crc32 csize) (zip-write-entry-body pool /dev/zipout entry-source entry-name rsize method strategy memlevel huffcodes topic)])
                    (if (not seekable?)
                        (if (and zip64-format?)
                            (let ([ds (make-zip64-data-descriptor #:crc32 crc32 #:rsize rsize #:csize csize)])
                              (write-zip64-data-descriptor ds /dev/zipout)
                              (zip-update-csize64 /dev/zmiout csize (offsetof-zipinfo-zip64 'csize)))
                            (let ([ds (make-zip-data-descriptor #:crc32 crc32 #:rsize (assert rsize index?) #:csize (assert csize index?))])
                              (write-zip-data-descriptor ds /dev/zipout)))
                        (let ([end-of-body-position (file-position /dev/zipout)])
                          (file-position /dev/zipout (+ position (offsetof-zip-file #| self-local |# 'crc32)))
                          (if (and zip64-format?)
                              (let ([ds (make-zip-data-descriptor #:crc32 crc32 #:rsize 0xFF32 #:csize 0xFF32)]
                                    [csize64-offset (offsetof-zipinfo-zip64 'csize)])
                                (write-zip-data-descriptor ds /dev/zipout)
                                (zip-update-csize64 /dev/zipout csize (+ position (offsetof-zip-file self-local 'metainfo) csize64-offset))
                                (zip-update-csize64 /dev/zmiout csize csize64-offset))
                              (let ([ds (make-zip-data-descriptor #:crc32 crc32 #:rsize (assert rsize index?) #:csize (assert csize index?))])
                                (write-zip-data-descriptor ds /dev/zipout)))
                          (file-position /dev/zipout end-of-body-position)))
                    (values crc32 csize))]))

    (make-zip-directory #:csystem pkzip-host-system #:cversion pkzip-digimon-version #:esystem pkzip-extraction-system #:eversion extraction-version
                        #:filename entry-name #:relative-offset (assert (if (not zip64-format?) position 0xFF32) index?)
                        #:crc32 crc32 #:rsize (assert (if (not zip64-format?) rsize 0xFF32) index?) #:csize (assert (if (not zip64-format?) csize 0xFF32) index?)
                        #:gpflag flag #:compression method #:mdate mdate #:mtime mtime
                        #:internal-attributes (if (archive-entry-ascii? entry) #b1 #b0)
                        #:external-attributes (zip-permission-attribute (archive-entry-permission entry) (not regular-file?))
                        #:metainfo (get-output-bytes /dev/zmiout #false) #:comment comment)))

(define zip-write-entry-body : (->* (Bytes Output-Port (U Bytes Path) String Natural ZIP-Compression-Method ZIP-Strategy Positive-Byte Symbol Symbol)
                                    ((Option Natural))
                                    (Values Index Natural))
  (lambda [pool /dev/stdout source entry-name total method strategy memlevel huffcodes topic [seek #false]]
    (define anchor : Natural
      (cond [(not seek) (file-position /dev/stdout)]
            [else (file-position /dev/stdout seek) seek]))

    (define /dev/zipout : Output-Port
      (case method
        [(deflated) (open-output-deflated-block /dev/stdout strategy #false #:huffman-codes huffcodes #:memory-level memlevel #:name entry-name)]
        [else /dev/stdout]))

    (define-values (_ crc32)
      (zip-entry-copy (open-progress-input-port (if (bytes? source) (open-input-memory source) (open-input-file source))
                                                topic entry-name (default-archive-entry-progress-handler) total)
                      /dev/zipout pool
                      
                      ; some systems may limit the maximum number of open files
                      ; if exception escapes, constructed ports would be closed by the custodian
                      #:close-input-port? #true
                      
                      ; keep consistent with other kind of sources
                      #:flush-output-port? #false))
    
    ; by design, all output ports associated with compression methods should follow the convention:
    ;   never mark the block with the FINAL flag when manually invoking `flush-output`,
    ;   and let the `close-output-port` flush and terminate the bitstream.
    ; as such, only do flushing on non-compression output port,
    ;   and we would have a chance to save some bytes for another empty final block
    ;   if the bitstream isn't block aligned.
    (if (eq? /dev/stdout /dev/zipout)
        (flush-output /dev/zipout)
        (close-output-port /dev/zipout))
    
    (values crc32
            (max 0 (- (file-position /dev/stdout)
                      anchor)))))

(define zip-write-directories : (->* (Output-Port (Option String) (Listof (Option ZIP-Directory)) Boolean) ((Option Natural)) Void)
  (lambda [/dev/zipout comment cdirs force-zip64? [at-position #false]]
    (define cdoffset : Natural
      (cond [(not at-position) (file-position /dev/zipout)]
            [else (file-position /dev/zipout at-position) at-position]))
    
    (define-values (cdsize count)
      (for/fold ([size : Natural 0] [count : Natural 0])
                ([cdir (in-list cdirs)] #:when cdir)
        (values (+ size (write-zip-directory cdir /dev/zipout)) (+ count 1))))

    #;(assert (- (file-position /dev/zipout) offset cdirsize) zero?)
    
    (define eocdr : ZIP-End-Of-Central-Directory
      (let ([offset32 (assert (if (not force-zip64?) (min cdoffset 0xFF32) 0xFF32) index?)]
            [cdsize32 (assert (min cdsize 0xFF32) index?)]
            [count32 (assert (min count 0xFF16) index?)])
        (make-zip-end-of-central-directory #:cdir-offset offset32 #:cdir-size cdsize32
                                           #:cdir-count count32 #:cdir-total count32
                                           #:comment (or comment ""))))

    (when (or (>= (zip-end-of-central-directory-cdir-offset eocdr) 0xFF32)
              (>= (zip-end-of-central-directory-cdir-size eocdr) 0xFF32)
              (>= (zip-end-of-central-directory-cdir-count eocdr) 0xFF16))
      (define eocdr64 : ZIP64-End-Of-Central-Directory
        (make-zip64-end-of-central-directory #:csystem pkzip-host-system #:cversion pkzip-digimon-version
                                             #:esystem pkzip-extraction-system #:eversion pkzip-extraction-version64
                                             #:cdir-offset cdoffset #:cdir-size cdsize #:cdir-count count #:cdir-total count))
      
      (define eocdl : ZIP64-End-Of-Central-Directory-Locator
        (make-zip64-end-of-central-directory-locator #:target-offset (file-position /dev/zipout)))

      (write-zip64-end-of-central-directory eocdr64 /dev/zipout)
      (write-zip64-end-of-central-directory-locator eocdl /dev/zipout))
    
    (write-zip-end-of-central-directory eocdr /dev/zipout)
    (flush-output /dev/zipout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-folder-entry? : (-> ZIP-Directory Boolean)
  (lambda [cdir]
    (let ([fname (zip-directory-filename cdir)])
      (eq? (string-ref fname (sub1 (string-length fname))) #\/))))

(define open-input-zip-entry : (-> Input-Port ZIP-Directory #:verify? Boolean Input-Port)
  (lambda [/dev/zipin cdir #:verify? verify?]
    (define-values (offset crc32 rsize csize zip64?) (zip-directory-data-descriptor cdir))
    (define ?zip (regexp-match #px"[^.]+$" (archive-port-name /dev/zipin)))
    (define port-name (format "~a://~a" (if (not ?zip) 'zip (car ?zip)) (zip-directory-filename cdir)))
        
    (file-position /dev/zipin offset)

    (let ([entry (read-zip-file /dev/zipin)])
      (when (and verify?) ; TODO
        (unless (string=? (zip-file-name entry) (zip-directory-filename cdir))
          (throw-check-error /dev/zipin 'open-input-zip-entry "entry name mismatch: [local]~a [central]~a"
                             (zip-file-name entry) (zip-directory-filename cdir)))

        (unless (= (zip-file-crc32 entry) (zip-directory-crc32 cdir))
          (throw-check-error /dev/zipin 'open-input-zip-entry "checksum mismatch: [local]~a [central]~a"
                             (number->string (zip-file-crc32 entry) 16) (number->string (zip-directory-crc32 cdir) 16)))))

    (when (zip-encrypted? (zip-directory-gpflag cdir))
      (throw-unsupported-error /dev/zipin 'open-input-zip-entry "encryped entry: ~a" (zip-directory-filename cdir)))

    (open-progress-input-port
     (case (zip-directory-compression cdir)
       [(deflated) (open-input-deflated-block /dev/zipin csize #false #:name port-name #:error-name 'zip:deflated)]
       [else (open-input-block /dev/zipin csize #false #:name port-name)])
     ((default-archive-progress-topic-resolver) /dev/zipin) (zip-directory-filename cdir)
     (default-archive-entry-progress-handler) csize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-entry-copy : (->* (Input-Port (U Output-Port (Listof Output-Port)))
                              (#:close-input-port? Boolean #:flush-output-port? Boolean (U Bytes Index False))
                              (Values Natural Index))
  (lambda [#:close-input-port? [close-in? #false] #:flush-output-port? [flush-out? #true]
           /dev/zipin /dev/zipout [pool0 4096]]
    (define-values (#{pool : Bytes} #{pool-size : Index})
      (cond [(bytes? pool0) (values pool0 (bytes-length pool0))]
            [(exact-positive-integer? pool0) (values (make-bytes pool0 0) pool0)]
            [else (values (make-bytes 4096 0) 4096)]))

    (define write-bytes-from-pool : (-> Nonnegative-Integer (U Index Void))
      (if (output-port? /dev/zipout)
          (λ [[read-size : Nonnegative-Integer]] (write-bytes pool /dev/zipout 0 read-size))
          (λ [[read-size : Nonnegative-Integer]] (for ([zipout (in-list /dev/zipout)]) (write-bytes pool zipout 0 read-size)))))

    (let copy-entry/checksum ([consumed : Natural 0]
                              [crc32 : Index 0])
      (define read-size : (U EOF Nonnegative-Integer Procedure)
        (read-bytes-avail! pool /dev/zipin 0 pool-size))
      
      (cond [(exact-positive-integer? read-size)
             (write-bytes-from-pool read-size)
             (copy-entry/checksum (+ consumed read-size) (checksum-crc32* pool crc32 0 read-size))]
            [(eof-object? read-size)
             (when (and flush-out?)
               (if (list? /dev/zipout)
                   (for-each flush-output /dev/zipout)
                   (flush-output /dev/zipout)))
             (when (and close-in?) (close-input-port /dev/zipin))
             (values consumed crc32)]
            [else ; deadcode. skip special values
             (copy-entry/checksum consumed crc32)]))))

(define zip-entry-copy/trap : (->* (Input-Port (U Output-Port (Listof Output-Port)) Natural Index) ((U Bytes Index False)) (U String True))
  (lambda [/dev/zipin /dev/zipout rSize CRC32 [pool0 4096]]
    (with-handlers ([exn:fail? exn-message])
      (let-values ([(rsize crc32) (zip-entry-copy /dev/zipin /dev/zipout pool0)])
        (cond [(not (= rSize rsize)) (format "bad size ~a (should be ~a, shortened by ~a)" rsize rSize (- rSize rsize))]
              [(not (= CRC32 crc32)) (format "bad checksum ~a (should be ~a)" (~hexstring crc32) (~hexstring CRC32))]
              [else #true])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-strategy-normalize : (->* (PKZIP-Strategy) ((Listof Any)) (Values ZIP-Strategy Symbol))
  (lambda [?strategy [options null]]
    (define all-options : (Listof Any) (cons ?strategy options))
    
    (values (or (for/or : (Option ZIP-Strategy) ([opt (in-list all-options)])
                  (cond [(zip-strategy? opt) opt]
                        [(index? opt) (zip-level->maybe-strategy opt)]
                        [(symbol? opt) (zip-name->maybe-strategy opt)]
                        [(and (pair? opt) (symbol? (car opt)) (index? (cdr opt)))
                         (zip-name->maybe-strategy (car opt) (cdr opt))]
                        [else #false]))
                (zip-default-preference))

            (or (for/or : (Option Symbol) ([opt (in-list all-options)])
                  (cond [(eq? opt 'fixed) opt]
                        [(eq? opt 'dynamic) opt]
                        [(eq? opt 'auto) opt]
                        [(eq? opt 'static) 'fixed]
                        [else #false]))
                'auto))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-update-csize64 : (-> Output-Port Natural Natural Byte)
  (lambda [/dev/zipout csize offset64]
    (file-position /dev/zipout offset64)
    (write-luint64 csize /dev/zipout)))
