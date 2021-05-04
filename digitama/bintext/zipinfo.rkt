#lang typed/racket/base

;;; https://support.pkware.com/home/pkzip/developer-tools/appnote/application-note-archives
;;; https://www.hanshq.net/zip.html
;;; collection://file/gunzip.rkt

(provide (all-defined-out))

(require "../../stdio.rkt")
(require "../../port.rkt")
(require "../../date.rkt")
(require "../../format.rkt")
(require "../../enumeration.rkt")

(require "../ioexn.rkt")
(require "../unsafe/ops.rkt")
(require "../unsafe/number.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%zip-entry : Index #x04034b50)
(define #%zip-cdirr : Index #x02014b50)
(define #%zip-eocdr : Index #x06054b50)

(define #%zip-?data : Index #x08074b50)
(define #%zip-digital : Index #x05054b50)

(define #%zip64-eocdr : Index #x06064b50)
(define #%zip64-eocdl : Index #x07064b50)

(define #%zip64-id : Index #x0001)

(define current-zip-entry : (Parameterof (U False ZIP-Directory)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* zip-system #:+> ZIP-System
  system->byte byte->system
  [FAT 0] [Amiga 1] [OpenVMS 2] [UNIX 3] [VM/CMS 4] [AtariST 5]
  [HPFS 6] [Macintosh 7] [Z-System 8] [CP/M 9] [NTFS 10]
  [MVS 11] [VSE 12] [AcornRISC 13] [VFAT 14] [AltMVS 15]
  [BeOS 16] [Tandem 17] [OS/400 18] [Darwin 19]
  [unused 20])

(define-enumeration* zip-compression-method #:+> ZIP-Compression-Method
  compression-method->index index->compression-method
  [stored 0]

  ;;; Deprecated ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  [shrunk 1] [reduced:1 2] [reduced:2 3] [reduced:3 4] [reduced:4 5] [imploded 6]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  [tokenizing 7] [deflated 8] [deflate64 9] [terse:old 10] [bzip2 12] [lzma 14] [cmpsc 16]
  [terse:new 18] [lz77 19] [zstd 93] [mp3 94] [xz 95] [jpeg 96] [wav 97] [ppmd 98] [AE-x 99])

(define-enumeration* zip-deflation-option #:+> ZIP-Deflation-Option
  deflation-option->index index->deflation-option
  [0 normal maximum fast superfast])

(define zip-encrypted? : (-> Index Boolean)
  (lambda [flag]
    (bitwise-bit-set? flag 0)))

(define zip-has-data-descriptor? : (-> Index Boolean)
  (lambda [flag]
    (bitwise-bit-set? flag 3)))

(define zip-clear-data-descriptor-flag : (-> Index Index)
  (lambda [flag]
    (bitwise-and flag #b1111111111110111)))

(define zip-deflation-option : (-> Index ZIP-Deflation-Option)
  (lambda [flag]
    (index->deflation-option (bitwise-bit-field flag 1 3) 'normal)))

(define zip-deflation-flag : (->* (ZIP-Deflation-Option Boolean) (Boolean) Index)
  (lambda [option has-data-descriptor? [encrypted? #false]]
    (bitwise-ior (if has-data-descriptor?  #b1000 #b0000)
                 (unsafe-idxlshift (deflation-option->index option) 1)
                 (if encrypted? #b1 #b0))))

(define zip-permission-attribute : (->* (Nonnegative-Fixnum) (Boolean) Index)
  (lambda [permission [dir? #false]]
    (define dos : Byte (if dir? #x10 0))
    (define unix : Nonnegative-Fixnum (bitwise-ior (if dir? #o40000 0) permission))

    (bitwise-ior dos (unsafe-idxlshift unix 16))))

(define zip-entry-modify-datetime : (-> Integer (Values Index Index))
  (lambda [utc-s]
    (utc-seconds->msdos-datetime utc-s #true)))

(define zip-entry-modify-seconds : (-> Index Index Natural)
  (lambda [mdate mtime]
    (msdos-datetime->utc-seconds mdate mtime #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct zip-file : ZIP-File ; the size of an zip entry should not exceed 64K
  ([signature : LUInt32 #:signature #%zip-entry]
   [eversion : Byte] ; version = major * 10 + minor
   [esystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16 #:radix 2]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [mtime : LUInt16]
   [mdate : LUInt16]
   [crc32 : LUInt32 #:default 0 #:radix 16]
   [csize : LUInt32 #:default 0 #:radix 16]
   [rsize : LUInt32 #:default 0 #:radix 16]
   [name-length : LUInt16]
   [metainfo-length : LUInt16]
   [name : (Localeof name-length)]
   [metainfo : (Bytesof metainfo-length) #:default #""]))

(define-binary-struct zip-data-descriptor : ZIP-Data-Descriptor
  ([signature : LUInt32 #:signature #%zip-?data #:omittable]
   [crc32 : LUInt32 #:radix 16]
   [csize : LUInt32 #:radix 16]
   [rsize : LUInt32 #:radix 16]))

(define-binary-struct zip64-data-descriptor : ZIP64-Data-Descriptor
  ([signature : LUInt32 #:signature #%zip-?data #:omittable]
   [crc32 : LUInt32 #:radix 16]
   [csize : LUInt64 #:radix 16]
   [rsize : LUInt64 #:radix 16]))

(define-binary-struct zip-directory : ZIP-Directory
  ([signature : LUInt32 #:signature #%zip-cdirr]
   [cversion : Byte]
   [csystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [eversion : Byte]
   [esystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16 #:radix 2]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [mtime : LUInt16]
   [mdate : LUInt16]
   [crc32 : LUInt32 #:radix 16]
   [csize : LUInt32 #:radix 16]
   [rsize : LUInt32 #:radix 16]
   [filename-length : LUInt16]
   [metainfo-length : LUInt16]
   [comment-length : LUInt16]
   [disk-number : LUInt16 #:default 0]
   [internal-attributes : LUInt16]
   [external-attributes : LUInt32]
   [relative-offset : LUInt32 #:radix 16]
   [filename : (Localeof filename-length)]
   [metainfo : (Bytesof metainfo-length) #:default #""]
   [comment : (Stringof comment-length)]))

(define-binary-struct zip64-end-of-central-directory : ZIP64-End-Of-Central-Directory
  ([signature : LUInt32 #:signature #%zip64-eocdr]
   [self-size : LUInt64 #:default (sizeof-zip64-end-of-central-directory)]
   [cversion : Byte]
   [csystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [eversion : Byte]
   [esystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [disk-idx : LUInt32 #:default 0]         ; Number of this disk (for multi-file-zip which is rarely used these days).
   [cdir0-disk-idx : LUInt32 #:default 0]   ; Number of the disk in which the central directory starts
   [cdir-count : LUInt64]                   ; Number of entries on this disk
   [cdir-total : LUInt64]                   ; Number of all entries
   [cdir-size : LUInt64]                    ; Size in bytes of the central directory
   [cdir-offset : LUInt64]                  ; Offset of the central directory section
   #;[reserved : (Stringof self-size #| a new feature is required to implement this |#)]))

(define-binary-struct zip64-end-of-central-directory-locator : ZIP64-End-Of-Central-Directory-Locator
  ([signature : LUInt32 #:signature #%zip64-eocdl]
   [eocdir0-disk-idx : LUInt32 #:default 0] ; Number of the disk in which the zip64 end of central directory starts
   [target-offset : LUInt64]                ; Offset of the zip64 end of central directory
   [disk-total : LUInt32 #:default 1]))     ; Number of all disks

(define-binary-struct zip-end-of-central-directory : ZIP-End-Of-Central-Directory
  ([signature : LUInt32 #:signature #%zip-eocdr]
   [disk-idx : LUInt16 #:default 0]         ; Number of this disk (for multi-file zip which is rarely used these days)
   [cdir0-disk-idx : LUInt16 #:default 0]   ; Number of the disk in which the central directory starts
   [cdir-count : LUInt16]                   ; Number of entries on this disk
   [cdir-total : LUInt16]                   ; Number of all entries
   [cdir-size : LUInt32]                    ; Size in bytes of the central directory
   [cdir-offset : LUInt32]                  ; Offset of the central directory section
   [comment : (LNString 2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensible data fields, 0x0 - 0x31 are reserved by PKWARE

(struct zip-metainfo () #:type-name ZIP-Metainfo #:transparent)

(define-binary-struct zip-metadata : ZIP-Metadata
  ([id : LUInt16 #:radix 16]
   [size : LUInt16]
   [body : (Bytesof size)]))

(define-binary-struct [zip64-extended-info zip-metainfo] : ZIP64-Extended-Info
  ([id : LUInt16 #:signature #%zip64-id]
   [size : LUInt16 #:default 28]

   [rsize : LUInt64 #:radix 16]
   [csize : LUInt64 #:radix 16]
   
   ; for central directories
   [relative-offset : LUInt64 #:default 0 #:radix 16]
   [disk-idx : LUInt32 #:default 0]))

(define read-zip-metadatas : (-> Bytes (Listof ZIP-Metadata))
  (lambda [block]
    (define total : Index (bytes-length block))
    (define /dev/zipin : Input-Port (open-input-memory block))
    
    (let read-metadata ([satad : (Listof ZIP-Metadata) null]
                        [idx : Nonnegative-Fixnum 0])
      (cond [(>= idx total) (reverse satad)]
            [else (let ([md (read-zip-metadata /dev/zipin)])
                    (read-metadata (cons md satad) (+ idx 4 (zip-metadata-size md))))]))))

(define read-zip-metainfos : (-> Bytes ZIP-Directory (Listof (Pairof Index ZIP-Metainfo)))
  (lambda [block cdir]
    (define /dev/zipin : Input-Port (open-input-bytes block))
    
    (let read-metainfo ([sofni : (Listof (Pairof Index ZIP-Metainfo)) null]
                        [idx : Nonnegative-Fixnum 0])
      (define meta-id (peek-luint16 /dev/zipin))
      (define meta-size (peek-luint16 /dev/zipin 2))
      
      (cond [(not (and meta-id meta-size)) (reverse sofni)]
            [else (let ([idx++ (+ (unsafe-idx+ idx 4) meta-size)])
                    (define maybe-info
                      (or (zip-load-metainfo /dev/zipin meta-id cdir)
                          (port-seek /dev/zipin idx++)))

                    (read-metainfo (if (zip-metainfo? maybe-info)
                                       (cons (cons meta-id maybe-info) sofni)
                                       sofni)
                                   idx++))]))))

(define read-zip-metainfo : (-> Bytes Index (U ZIP-Directory ZIP-File) (Option ZIP-Metainfo))
  (lambda [block id e]
    (define pos : (Option Index) (seek-zip-metainfo block id))
    
    (and pos
         (let ([/dev/zipin (open-input-memory block pos)])
           (zip-load-metainfo /dev/zipin id e)))))

(define seek-zip-metainfo : (-> Bytes Index (Option Index))
  (lambda [block id]
    (define total : Index (bytes-length block))
    
    (let seek-metainfo ([idx : Nonnegative-Fixnum 0])
      (and (< idx total)
           (let ([idx+4 (+ idx 4)])
             (and (< idx+4 total)
                  (let ([meta-id (lsb-bytes->short block idx #false)])
                    (cond [(= id meta-id) idx]
                          [else (let ([meta-size (lsb-bytes->short block (+ idx 2) #false)])
                                  (seek-metainfo (+ meta-size idx+4)))]))))))))

(define remove-zip-metainfo : (-> Bytes Index Bytes)
  (lambda [block id]
    (define pos : (Option Index) (seek-zip-metainfo block id))

    (cond [(not pos) block]
          [else (let* ([meta-size (lsb-bytes->short block (+ pos 2) #false)]
                       [shorten-size (+ meta-size 4)]
                       [total (bytes-length block)])
                  (bytes-copy! block pos block (+ shorten-size pos) total)
                  (subbytes block 0 (- total shorten-size)))])))

(define zip-load-metainfo : (-> Input-Port Index (U ZIP-Directory ZIP-File) (Option ZIP-Metainfo))
  (lambda [/dev/zipin id e]
    (case id
      [(#x0001)
       (let*-values ([(id size) (values (read-luint16 /dev/zipin) (read-luint16 /dev/zipin))]
                     [(offset rsize csize disk-idx) (zip64-load-metainfo /dev/zipin e size)])
         (make-zip64-extended-info #:size size #:rsize rsize #:csize csize #:relative-offset offset #:disk-idx disk-idx))]
      ;[(#x0007) (read-zip-av-info /dev/zipin)]
      ;[(#x0008) (read-zip-language-info /dev/zipin)]
      ;[(#x0009) (read-zip-os/2-info /dev/zipin)]
      ;[(#x000A) (read-zip-ntfs-info /dev/zipin)]
      ;[(#x000C) (read-zip-openvms-info /dev/zipin)]
      ;[(#x000D) (read-zip-unix-info /dev/zipin)]
      ;[(#x000E) (read-zip-fork-descriptor /dev/zipin)]
      ;[(#x000F) (read-zip-patch-descriptor /dev/zipin)]
      ;[(#x0014) (read-zip-pkcs#7-info /dev/zipin)]
      ;[(#x0015) (read-zip-file-x509-info /dev/zipin)]
      ;[(#x0016) (read-zip-directory-x509-info /dev/zipin)]
      ;[(#x0017) (read-zip-strong-encryption-header /dev/zipin)]
      ;[(#x0018) (read-zip-record-management-controls /dev/zipin)]
      ;[(#x0019) (read-zip-pkcs#7-list /dev/zipin)]
      ;[(#x0020) (read-zip-timestamp-record /dev/zipin)]
      ;[(#x0021) (read-zip-policy-decryption-key-record /dev/zipin)]
      ;[(#x0022) (read-zip-smartcrypt-key-provider-record /dev/zipin)]
      ;[(#x0023) (read-zip-smartcrypt-policy-key-record /dev/zipin)]
      ;[(#x0065) (read-zip-ibm-attributes/raw /dev/zipin)]
      ;[(#x0066) (read-zip-ibm-attributes/compressed /dev/zipin)]
      ;[(#xA220) (read-moxml-growth-hint /dev/zipin)]
      ;[(#x4690) (read-zip-poszip-info /dev/zipin)]
      [else #false])))

(define zip64-load-metainfo : (-> Input-Port (U ZIP-Directory ZIP-File) Index (Values Natural Natural Natural Index))
  (lambda [/dev/zipin e size]
    (define-values (e-offset e-rsize e-csize)
      (if (zip-directory? e)
          (values (zip-directory-relative-offset e) (zip-directory-rsize e) (zip-directory-csize e))
          (values 0 (zip-file-rsize e) (zip-file-csize e))))
    
    (let*-values ([(rsize s) (if (>= e-rsize 0xFF32) (values (read-luint64 /dev/zipin) 8) (values e-rsize 0))]
                  [(csize s) (if (>= e-csize 0xFF32) (values (read-luint64 /dev/zipin) (+ s 8)) (values e-csize s))]
                  [(offset s) (if (>= e-offset 0xFF32) (values (read-luint64 /dev/zipin) (+ s 8)) (values e-offset s))])
      (values offset rsize csize
              (cond [(>= s size) 0]
                    [else (read-luint32 /dev/zipin)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-seek-local-file-signature : (-> Input-Port (Option Natural))
  (lambda [/dev/zipin]
    (let seek ([pos : Natural (file-position /dev/zipin)])
      (define sig : (Option Index) (peek-luint32 /dev/zipin))
      (cond [(eq? sig #%zip-entry) (file-position /dev/zipin)]
            [(eq? sig #%zip-cdirr) #false]
            [(eq? sig #%zip-eocdr) #false]
            [else (and sig (seek (port-seek /dev/zipin (+ pos 1))))]))))

(define zip-seek-signature : (->* (Input-Port) ((Option Natural)) (Option Natural))
  (lambda [/dev/zipin [comment-maxsize #false]]
    (define start : Natural (port-seek /dev/zipin (- (sizeof-zip-end-of-central-directory))))
    (define end : Natural (if (not comment-maxsize) 0 (max (- start comment-maxsize) 0)))
    
    (let seek ([pos : Natural start])
      (cond [(eq? (peek-luint32 /dev/zipin) #%zip-eocdr) pos]
            [else (and (> pos end) (seek (port-seek /dev/zipin (- pos 1))))]))))

(define zip-seek-signature* : (->* (Input-Port (U Symbol Procedure)) ((Option Natural)) Natural)
  (lambda [/dev/zipin source [comment-maxsize #false]]
    (define maybe-sigoff : (Option Natural) (zip-seek-signature /dev/zipin comment-maxsize))

    (cond [(not maybe-sigoff) (throw-signature-error /dev/zipin source "not a ZIP file")]
          [else maybe-sigoff])))

(define zip-seek-central-directory-section : (->* (Input-Port (U Symbol Procedure)) ((Option Natural)) (Values Natural Natural String))
  (lambda [/dev/zipin source [comment-maxsize #false]]
    (define eocdir-idx : Natural (zip-seek-signature* /dev/zipin source comment-maxsize))
    (define eocdir : ZIP-End-Of-Central-Directory (read-zip-end-of-central-directory /dev/zipin))
    (define offset : Natural (zip-end-of-central-directory-cdir-offset eocdir))
    (define cdsize : Index (zip-end-of-central-directory-cdir-size eocdir))
    (define count : Index (zip-end-of-central-directory-cdir-count eocdir))
    (define comment : String (zip-end-of-central-directory-comment eocdir))

    (if (or (>= offset 0xFF32) (>= cdsize 0xFF32) (>= count 0xFF16))
        (let ([locator-pos (- eocdir-idx (sizeof-zip64-end-of-central-directory-locator))])
          (when (< locator-pos 0)
            (throw-check-error /dev/zipin source "invalid ZIP64 file"))

          (let* ([locator (read-zip64-end-of-central-directory-locator /dev/zipin locator-pos)]
                 [eocdir64 (read-zip64-end-of-central-directory /dev/zipin (zip64-end-of-central-directory-locator-target-offset locator))]
                 [offset64 (if (>= offset 0xFF32) (zip64-end-of-central-directory-cdir-offset eocdir64) offset)])
            (values offset64 (+ offset64 (if (>= cdsize 0xFF32) (zip64-end-of-central-directory-cdir-size eocdir64) cdsize)) comment)))
        (values offset (+ offset cdsize) comment))))

(define zip-directory-data-descriptor : (-> ZIP-Directory (Values Natural Index Natural Natural Boolean))
  (lambda [cdir]
    (define offset : Natural (zip-directory-relative-offset cdir))
    (define crc32 : Index (zip-directory-crc32 cdir))
    (define rsize : Index (zip-directory-rsize cdir))
    (define csize : Index (zip-directory-csize cdir))
    
    (if (or (>= offset 0xFF32) (>= rsize 0xFF32) (>= csize 0xFF32))
        (let ([zip64-info (read-zip-metainfo (zip-directory-metainfo cdir) #%zip64-id cdir)])
          (if (zip64-extended-info? zip64-info)
              (values (zip64-extended-info-relative-offset zip64-info) crc32
                      (zip64-extended-info-rsize zip64-info) (zip64-extended-info-csize zip64-info) #true)
              (values offset crc32 rsize csize #false)))
        (values offset crc32 rsize csize #false))))

(define zip-directory-relocate : (->* (ZIP-Directory Natural) (#:clear-data-descriptor-flag? Boolean) (Values ZIP-Directory Natural Natural))
  (lambda [cdir pos #:clear-data-descriptor-flag? [clear-datadesc? #true]]
    (define gpflag : Index (zip-directory-gpflag cdir))
    (define nflag : Index (if (and clear-datadesc? (zip-has-data-descriptor? gpflag)) (zip-clear-data-descriptor-flag gpflag) gpflag))
    (define-values (offset crc32 rsize csize zip64?) (zip-directory-data-descriptor cdir))
    (define block : Bytes (zip-directory-metainfo cdir))
    
    (values (or (and (< pos 0xFF32)
                     (cond [(not zip64?)
                            (remake-zip-directory cdir #:gpflag nflag #:relative-offset pos)]
                           [(and (< rsize 0xFF32) (< csize 0xFF32))
                            (remake-zip-directory cdir #:gpflag nflag #:relative-offset pos #:metainfo (remove-zip-metainfo block #%zip64-id))]
                           [else #false]))

                (remake-zip-directory cdir #:rsize 0xFF32 #:csize 0xFF32 #:relative-offset 0xFF32 #:gpflag nflag
                                      ; TODO: squeeze out every single byte based on the trick of zip64 extended data
                                      #:metainfo (let ([md64 (make-zip64-extended-info #:csize csize #:rsize rsize #:relative-offset pos)])
                                                   (bytes-append (zip64-extended-info->bytes md64) (remove-zip-metainfo block #%zip64-id)))))
            offset csize)))

(define zip-directory-entry-section : (-> Input-Port ZIP-Directory (Values Natural Natural))
  (lambda [/dev/zipin cdir]
    (define-values (offset crc32 rsize csize zip64?) (zip-directory-data-descriptor cdir))

    (file-position /dev/zipin offset)

    (let ([file (read-zip-file /dev/zipin)])
      (file-position /dev/zipin (+ offset (sizeof-zip-file file) csize))
      (when (zip-has-data-descriptor? (zip-file-gpflag file))
        (if (not zip64?)
            (read-zip-data-descriptor /dev/zipin)
            (read-zip64-data-descriptor /dev/zipin)))
      (values offset (file-position /dev/zipin)))))

(define zip-directory->local-file-entry : (-> ZIP-Directory ZIP-File)
  (lambda [cdir]
    (define-values (offset crc32 rsize csize zip64?) (zip-directory-data-descriptor cdir))
    (define block : Bytes (zip-directory-metainfo cdir))

    (if (or (not zip64?) (>= rsize 0xFF32) (>= csize 0xFF32))
        (make-zip-file #:esystem (zip-directory-esystem cdir) #:eversion (zip-directory-eversion cdir)
                       #:name (zip-directory-filename cdir) #:gpflag (zip-directory-gpflag cdir) #:compression (zip-directory-compression cdir)
                       #:mdate (zip-directory-mdate cdir) #:mtime (zip-directory-mtime cdir)
                       #:crc32 crc32 #:csize (zip-directory-csize cdir) #:rsize (zip-directory-rsize cdir) #:metainfo block)

        (make-zip-file #:esystem (zip-directory-esystem cdir) #:eversion (zip-directory-eversion cdir)
                       #:name (zip-directory-filename cdir) #:gpflag (zip-directory-gpflag cdir) #:compression (zip-directory-compression cdir)
                       #:mdate (zip-directory-mdate cdir) #:mtime (zip-directory-mtime cdir)
                       #:crc32 crc32 #:csize csize #:rsize rsize #:metainfo (remove-zip-metainfo block #%zip64-id)))))
  
(define zip-file-data-descriptor : (-> ZIP-File (Values Index Natural Natural))
  (lambda [file]
    (define crc32 : Index (zip-file-crc32 file))
    (define rsize : Index (zip-file-rsize file))
    (define csize : Index (zip-file-csize file))

    (if (or (>= rsize 0xFF32) (>= csize 0xFF32))
        (let ([zip64-info (read-zip-metainfo (zip-file-metainfo file) #%zip64-id file)])
          (if (zip64-extended-info? zip64-info)
              (values crc32 (zip64-extended-info-rsize zip64-info) (zip64-extended-info-csize zip64-info))
              (values crc32 rsize csize)))
        (values crc32 rsize csize))))

(define zip-entry-metrics : (-> (U ZIP-Directory ZIP-File) (Values String Natural Natural))
  (lambda [e]
    (if (zip-directory? e)
        (let-values ([(_ __ rsize csize _?) (zip-directory-data-descriptor e)])
          (values (zip-directory-filename e) rsize csize))
        (let-values ([(_ rsize csize) (zip-file-data-descriptor e)])
          (values (zip-file-name e) rsize csize)))))

(define read-zip-file* : (->* (Input-Port) ((Option Index)) ZIP-File)
  (lambda [/dev/zipin [posoff #false]]
    (define-values (entry dr-size) (read-zip-file** /dev/zipin posoff))
    entry))

(define read-zip-file** : (->* (Input-Port) ((Option Index)) (Values ZIP-File Byte))
  (lambda [/dev/zipin [posoff #false]]
    (values (read-zip-file /dev/zipin posoff) 0)))
