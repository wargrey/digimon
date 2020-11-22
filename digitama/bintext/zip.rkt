#lang typed/racket/base

; http://www.pkware.com/company/standards/appnote
; https://www.hanshq.net/zip.html
; collection: file/gunzip.rkt

(provide (all-defined-out))

(require "../../stdio.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-header zip-entry : ZIP-Entry
  ([signature : LUInt32 #x04034b50]
   [version : LUInt16]
   [flags : LUInt16]
   [compression : LUInt16]
   [mtime : LUInt16]
   [mdate : LUInt16]
   [crc32 : LUInt32]
   [csize : LUInt32]
   [rsize : LUInt32]
   [file-name-length : LUInt16]
   [private-length : LUInt16]
   [file-name : (Bytesof file-name-length)]
   [privates : (Bytesof private-length)]))
