#lang typed/racket/base

(require digimon/stdio)

(require "../digitama/bintext/zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct stdio-entry : Stdio-Entry
  ([signature : MUInt32 #:signature #%zip-?data #:omittable]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [filename-length : LUInt16]
   [comment : (MNBytes 2)]
   [filename : (Stringof filename-length)]
   [os : (#:enum Byte system->byte byte->system #:fallback 'unused)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define entry (make-stdio-entry #:os 'Macintosh #:compression 'stored #:filename "二进制文件.zip" #:comment #"read and write binary files"))

  (default-stdout-all-fields? #false)
  
  (bytes->stdio-entry (stdio-entry->bytes entry))

  (display-stdio-entry entry)
  (sizeof-stdio-entry entry)
  
  (for/list : (Listof Any) ([field (list 'signature 'compression 'filename-length 'comment 'filename 'os)])
    (list field
          (offsetof-stdio-entry field)
          (offsetof-stdio-entry entry field))))
