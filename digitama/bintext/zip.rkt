#lang typed/racket/base

; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-2.0.txt
; https://www.hanshq.net/zip.html
; collection: file/gunzip.rkt

(provide (all-defined-out))

(require typed/racket/date)

(require "../../stdio.rkt")
(require "../../port.rkt")
(require "../../enumeration.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%zip-entry : Index #x04034b50)
(define #%zip-cdirr : Index #x02014b50)
(define #%zip-eocdr : Index #x06054b50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* zip-system #:+> ZIP-System
  system->byte byte->system
  [FAT 0] [Amiga 1] [VAX/MS 2] [Unix 3] [VM/CMS 4] [AtariST 5]
  [HPFS 6] [Macintosh 7] [Z-System 8] [CP/M 9] [unused 10])

(define-enumeration* zip-compression-method #:+> ZIP-Compression-Method
  compression-method->index index->compression-method
  [0 stored shrunk
     reduced:1 reduced:2 reduced:3 reduced:4
     imploded tokenizing
     deflated])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct zip-entry : ZIP-Entry
  ([signature : LUInt32 #:signature #%zip-entry]
   [extract-version : Byte]
   [extract-os : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [lmtime : LUInt16]
   [lmdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [filename-length : LUInt16]
   [private-length : LUInt16]
   [filename : (Stringof filename-length)]
   [privates : (Bytesof private-length)]))

(define-binary-struct zip-directory : ZIP-Directory
  ([signature : LUInt32 #:signature #%zip-cdirr]
   [create-version : Byte]
   [create-os : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [extract-version : Byte]
   [extract-os : (#:enum Byte system->byte byte->system #:fallback 'unused)]
   [gpflag : LUInt16]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [lmtime : LUInt16]
   [lmdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [filename-length : LUInt16]
   [private-length : LUInt16]
   [comment-length : LUInt16]
   [disk-number : LUInt16]
   [internal-attributes : LUInt16]
   [external-attributes : LUInt32]
   [relative-offset : LUInt32]
   [filename : (Stringof filename-length)]
   [privates : (Bytesof private-length)]
   [comment : (Stringof comment-length)]))

(define-binary-struct zip-end-of-central-directory : ZIP-End-Of-Central-Directory
  ([signature : LUInt32 #:signature #%zip-eocdr]
   [disk-idx : LUInt16]       ; Number of this disk (for multi-file-zip which is rarely used these days).
   [cdir0-disk-idx : LUInt16] ; Number of the disk in which the central directory starts
   [entry-count : LUInt16]    ; Number of entries on this disk 
   [entry-total : LUInt16]    ; Number of all entries
   [cdir-size : LUInt32]      ; Size in bytes of the central directory
   [cdir-offset : LUInt32]    ; Offset of the central directory
   [comment : (LNString 2)]))

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
