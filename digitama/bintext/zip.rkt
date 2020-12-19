#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "zipinfo.rkt")
(require "deflate.rkt")
(require "archive.rkt")

(require "../ioexn.rkt")

(require "../../port.rkt")
(require "../../date.rkt")
(require "../../checksum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pkzip-compression-methods : (Listof ZIP-Compression-Method) (list 'stored 'deflated))
(define pkzip-digimon-version : Byte 20)
(define pkzip-extract-system : ZIP-System 'FAT)
(define pkzip-extract-version : Byte 20)

(define pkzip-host-system : ZIP-System
  (case (system-type 'os)
    [(macosx) 'Darwin]
    [(unix) 'UNIX]
    [else 'NTFS]))

(define pkzip-default-suffixes : (Listof Symbol) (list 'zip 'Z 'zoo 'arc 'lzh 'arj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-write-entry : (-> Output-Port Archive-Entry (Option Path-String) (Option Path-String) Regexp Boolean (Option ZIP-Directory))
  (lambda [/dev/zipout entry root zip-root px:suffix seekable?]
    (define entry-source : (U Bytes Path) (archive-entry-src entry))
    (define file? : Boolean (or (bytes? entry-source) (file-exists? entry-source)))
    (define entry-name : String (archive-entry-filename (archive-entry-name entry) root zip-root))
    (define-values (mdate mtime) (utc-seconds->msdos-datetime (archive-entry-mtime entry)))

    (define method : ZIP-Compression-Method
      (let check-method ([ms : (Listof Symbol) (archive-entry-methods entry)])
        (cond [(pair? ms)
               (let ([candidates (memq (car ms) pkzip-compression-methods)])
                 (cond [(not candidates) (check-method (cdr ms))]
                       [else (car candidates)]))]
              [(regexp-match? px:suffix entry-name) 'stored]
              [else (values 'deflated)])))

    (define flag : Index
      (case method
        [(deflated)
         (let ([dops (filter zip-deflation-option? (archive-entry-options entry))])
           (zip-deflation-flag (if (pair? dops) (car dops) 'normal) (not seekable?)))]
        [else 0]))

    (define position : Natural (file-position /dev/zipout))
    (define self-local : ZIP-Entry
      (make-zip-entry #:extract-system pkzip-extract-system #:extract-version pkzip-extract-version
                      #:filename (if (not file?) (path->string (path->directory-path entry-name)) entry-name)
                      #:gpflag flag #:compression method #:mdate mdate #:mtime mtime))

    (define sizes : ZIP-Data-Descriptor
      (cond [(not file?)
             (write-zip-entry self-local /dev/zipout)
             (make-zip-data-descriptor #:crc32 0 #:csize 0 #:rsize 0)]
            [(not seekable?)
             (write-zip-entry self-local /dev/zipout)
             (let ([desc (zip-write-file-entry-content /dev/zipout entry-source method)])
               (write-zip-data-descriptor desc /dev/zipout)
               desc)]
            [else
             (let* ([self-size (sizeof-zip-entry self-local)]
                    [desc (zip-write-file-entry-content /dev/zipout entry-source method (+ position self-size))])
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
                        #:external-attributes (zip-permission-attribute (archive-entry-permission entry) (not file?))
                        #:comment (or (archive-entry-comment entry) ""))))

(define zip-write-file-entry-content : (->* (Output-Port (U Bytes Path) ZIP-Compression-Method) ((Option Natural)) ZIP-Data-Descriptor)
  (let* ([pool-size : Index 4096]
         [pool : Bytes (make-bytes pool-size)])
    (lambda [/dev/zipout source method [seek #false]]
      (when (exact-integer? seek)
        (file-position /dev/zipout seek))
      
      (define-values (crc32 csize rsize)
        (case method
          [else #| stored |#
           (if (bytes? source)
               (let ([size (write-bytes source /dev/zipout)])
                 (values (checksum-crc32 source 0 size) size size))
               (let store : (Values Index Natural Natural) ([/dev/zipin (open-input-file source)]
                                                            [size : Natural 0]
                                                            [crc32 : Index 0])
                 (define read : (U EOF Positive-Integer) (read-bytes! pool /dev/zipin 0 pool-size))
                 (cond [(eof-object? read) (values crc32 size size)]
                       [else (let ([wrote (write-bytes pool /dev/zipout 0 read)])
                               (store /dev/zipin (+ size wrote) (checksum-crc32* pool crc32 0 wrote)))])))]))

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
(define zip-path-normalize : (-> Path-String String)
  (let ([pxwin-separator #rx"\\\\"])
    (lambda [filename]
      (cond [(string? filename) (regexp-replace* pxwin-separator filename "/")]
            [else (case (path-convention-type filename)
                    [(windows) (regexp-replace* pxwin-separator (path->string filename) "/")]
                    [else (path->string filename)])]))))
