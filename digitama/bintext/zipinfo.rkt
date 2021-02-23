#lang typed/racket/base

;;; https://support.pkware.com/home/pkzip/developer-tools/appnote/application-note-archives
;;; https://www.hanshq.net/zip.html
;;; collection://file/gunzip.rkt

(provide (all-defined-out))

(require "../../stdio.rkt")
(require "../../port.rkt")
(require "../../enumeration.rkt")

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%zip-entry : Index #x04034b50)
(define #%zip-cdirr : Index #x02014b50)
(define #%zip-eocdr : Index #x06054b50)

(define current-zip-entry : (Parameterof (U False ZIP-Entry ZIP-Directory)) (make-parameter #false))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct zip-entry : ZIP-Entry
  ([signature : LUInt32 #:signature #%zip-entry]
   [extract-version : Byte] ; version = major * 10 + minor
   [extract-system : (#:enum Byte system->byte byte->system #:fallback 'unused)]
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

(define-binary-struct zip-directory : ZIP-Directory
  ([signature : LUInt32 #:signature #%zip-cdirr]
   [create-version : Byte]
   [create-system : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [extract-version : Byte]
   [extract-system : (#:enum Byte system->byte byte->system #:fallback 'unused)]
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

(define-binary-struct zip-end-of-central-directory : ZIP-End-Of-Central-Directory
  ([signature : LUInt32 #:signature #%zip-eocdr]
   [disk-idx : LUInt16 #:default 0]       ; Number of this disk (for multi-file-zip which is rarely used these days).
   [cdir0-disk-idx : LUInt16 #:default 0] ; Number of the disk in which the central directory starts
   [entry-count : LUInt16]                ; Number of entries on this disk
   [entry-total : LUInt16]                ; Number of all entries
   [cdir-size : LUInt32]                  ; Size in bytes of the central directory
   [cdir-offset : LUInt32]                ; Offset of the central directory section
   [comment : (LNString 2)]))

(define-binary-struct zip-data-descriptor : ZIP-Data-Descriptor
  ([crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]))

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

(define read-zip-entry* : (->* (Input-Port) ((Option Index)) ZIP-Entry)
  (lambda [/dev/zipin [posoff #false]]
    (define-values (entry dr-size) (read-zip-entry** /dev/zipin posoff))
    entry))

(define read-zip-entry** : (->* (Input-Port) ((Option Index)) (Values ZIP-Entry Byte))
  (lambda [/dev/zipin [posoff #false]]
    (values (read-zip-entry /dev/zipin posoff) 0)))
