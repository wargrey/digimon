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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%zip-entry : Index #x04034b50)
(define #%zip-cdirr : Index #x02014b50)
(define #%zip-eocdr : Index #x06054b50)

(define #%zip-?data : Index #x08074b50)
(define #%zip-digital : Index #x05054b50)

(define #%zip64-eocdr : Index #x06064b50)
(define #%zip64-eocdl : Index #x07064b50)

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
(define-binary-struct zip-entry : ZIP-Entry
  ([signature : LUInt32 #:signature #%zip-entry]
   [eversion : Byte] ; version = major * 10 + minor
   [esystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [mtime : LUInt16]
   [mdate : LUInt16]
   [crc32 : LUInt32 #:default 0]
   [csize : LUInt32 #:default 0]
   [rsize : LUInt32 #:default 0]
   [filename-length : LUInt16]
   [metainfo-length : LUInt16]
   [filename : (Localeof filename-length)]
   [metainfo : (Bytesof metainfo-length) #:default #""]))

(define-binary-struct zip-data-descriptor : ZIP-Data-Descriptor
  ([signature : LUInt32 #:signature #%zip-?data]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]))

(define-binary-struct zip64-data-descriptor : ZIP64-Data-Descriptor
  ([signature : LUInt32 #:signature #%zip-?data]
   [crc32 : LUInt32]
   [csize : LUInt64]
   [rsize : LUInt64]))

(define-binary-struct zip-directory : ZIP-Directory
  ([signature : LUInt32 #:signature #%zip-cdirr]
   [cversion : Byte]
   [csystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [eversion : Byte]
   [esystem : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [mtime : LUInt16]
   [mdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [filename-length : LUInt16]
   [metainfo-length : LUInt16]
   [comment-length : LUInt16]
   [disk-number : LUInt16 #:default 0]
   [internal-attributes : LUInt16]
   [external-attributes : LUInt32]
   [relative-offset : LUInt32]
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
   #;[reserved : (Stringof self-size)]))

(define-binary-struct zip64-end-of-central-directory-locator : ZIP64-End-Of-Central-Directory-Locator
  ([signature : LUInt32 #:signature #%zip64-eocdl]
   [eocdir0-disk-idx : LUInt32 #:default 0] ; Number of the disk in which the zip64 end of central directory starts
   [target-offset : LUInt64]                ; Offset of the zip64 end of central directory
   [disk-total : LUInt32 #:default 1]))     ; Number of all disks

(define-binary-struct zip-end-of-central-directory : ZIP-End-Of-Central-Directory
  ([signature : LUInt32 #:signature #%zip-eocdr]
   [disk-idx : LUInt16 #:default 0]         ; Number of this disk (for multi-file-zip which is rarely used these days).
   [cdir0-disk-idx : LUInt16 #:default 0]   ; Number of the disk in which the central directory starts
   [cdir-count : LUInt16]                   ; Number of entries on this disk
   [cdir-total : LUInt16]                   ; Number of all entries
   [cdir-size : LUInt32]                    ; Size in bytes of the central directory
   [cdir-offset : LUInt32]                  ; Offset of the central directory section
   [comment : (LNString 2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensible data fields, 0x0 - 0x31 are reserved by PKWARE

(struct zip-metainfo () #:type-name ZIP-Metainfo)

(define-binary-struct zip-metainfo-header : ZIP-Metainfo-Header
  ([id : LUInt16]
   [size : LUInt16]))

(define read-zip-metainfos : (-> Bytes (Listof ZIP-Metainfo))
  (lambda [block]
    (define total : Index (bytes-length block))
    (define /dev/zipin : Input-Port (open-input-bytes block))
    
    (let read-metainfo ([sofni : (Listof ZIP-Metainfo) null]
                        [idx : Nonnegative-Fixnum 0])
      (cond [(>= idx total) (reverse sofni)]
            [else (let ([header (read-zip-metainfo-header /dev/zipin)])
                    (define idx++ : Nonnegative-Fixnum
                      (+ (unsafe-idx+ idx (sizeof-zip-metainfo-header))
                         (zip-metainfo-header-size header)))
                    
                    (define maybe-info
                      (case (zip-metainfo-header-id header)
                        ;[(#x0001) (read-zip64-extended-info /dev/zipin)]
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
                        ;[(#x4690) (read-zip-poszip-info /dev/zipin)]
                        [else (port-seek /dev/zipin idx++)]))

                    (read-metainfo (if (zip-metainfo? maybe-info) (cons maybe-info sofni) sofni) idx++))]))))

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

    (if (or (>= offset #xFFFFFFFF) (>= cdsize #xFFFFFFFF) (>= count #xFFFF))
        (let ([locator-pos (- eocdir-idx (sizeof-zip64-end-of-central-directory-locator))])
          (when (< locator-pos 0)
            (throw-signature-error /dev/zipin source "invalid ZIP64 file"))

          (let* ([locator (read-zip64-end-of-central-directory-locator /dev/zipin locator-pos)]
                 [eocdir64 (read-zip64-end-of-central-directory /dev/zipin (zip64-end-of-central-directory-locator-target-offset locator))]
                 [offset64 (zip64-end-of-central-directory-cdir-offset eocdir64)])
            (values offset64 (+ offset64 (zip64-end-of-central-directory-cdir-size eocdir64)) comment)))
        (values offset (+ offset cdsize) comment))))

(define read-zip-entry* : (->* (Input-Port) ((Option Index)) ZIP-Entry)
  (lambda [/dev/zipin [posoff #false]]
    (define-values (entry dr-size) (read-zip-entry** /dev/zipin posoff))
    entry))

(define read-zip-entry** : (->* (Input-Port) ((Option Index)) (Values ZIP-Entry Byte))
  (lambda [/dev/zipin [posoff #false]]
    (values (read-zip-entry /dev/zipin posoff) 0)))
