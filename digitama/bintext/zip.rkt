#lang typed/racket/base

; https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-2.0.txt
; https://www.hanshq.net/zip.html
; collection: file/gunzip.rkt

(provide (all-defined-out))

(require "../../stdio.rkt")
(require "../../port.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%zip-entry : Index #x04034b50)
(define #%zip-cdirr : Index #x02014b50)
(define #%zip-eocdr : Index #x06054b50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-header zip-entry : ZIP-Entry
  ([signature : LUInt32 #%zip-entry]
   [version : LUInt16]
   [flags : LUInt16]
   [compression : LUInt16]
   [lmtime : LUInt16]
   [lmdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [file-name-length : LUInt16]
   [private-length : LUInt16]
   [file-name : (Bytesof file-name-length)]
   [privates : (Bytesof private-length)]))

(define-file-header zip-directory : ZIP-Directory
  ([signature : LUInt32 #%zip-cdirr]
   [maker : LUInt16]
   [version : LUInt16]
   [flags : LUInt16]
   [compression : LUInt16]
   [lmtime : LUInt16]
   [lmdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [file-name-length : LUInt16]
   [private-length : LUInt16]
   [comment-length : LUInt16]
   [disk-number : LUInt16]
   [internal-attributes : LUInt16]
   [external-attributes : LUInt32]
   [relative-offset : LUInt32]
   [file-name : (Bytesof file-name-length)]
   [privates : (Bytesof private-length)]
   [comment : (Bytesof comment-length)]))

(define-file-header zip-end-of-central-directory : ZIP-End-Of-Central-Directory
  ([signature : LUInt32 #%zip-eocdr]
   [disk-idx : LUInt16]       ; Number of this disk (for multi-file-zip which is rarely used these days).
   [cdir0-disk-idx : LUInt16] ; Number of the disk in which the central directory starts
   [entry-count : LUInt16]    ; Number of entries on this disk 
   [entry-total : LUInt16]    ; Number of all entries
   [cdir-size : LUInt32]      ; Size in bytes of the central directory
   [cdir-offset : LUInt32]    ; Offset of the central directory
   [comment : (LNBytes 2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-seek-signature : (->* (Input-Port) (Integer) Boolean)
  (lambda [/dev/zipin [comment-maxsize -1]]
    (define start : Natural (port-seek /dev/zipin (- zip-end-of-central-directory-size0)))
    (define end : Natural (if (< comment-maxsize 0) 0 (max (- start comment-maxsize) 0)))
    
    (let seek ([pos : Natural (port-seek /dev/zipin (- zip-end-of-central-directory-size0))])
      (or (eq? (peek-luint32 /dev/zipin) #%zip-eocdr)
          (and (> pos end)
               (seek (port-seek /dev/zipin (- pos 1))))))))
