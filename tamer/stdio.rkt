#lang typed/racket/base

(require digimon/stdio)

(require "../digitama/bintext/zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct stdio-entry : Stdio-Entry
  ([signature : MUInt32 #:signature #%zip-?data #:omittable]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [filename-length : LUInt16 #:datum-offset 1]
   [version : Byte #:default 1]
   [comment : (MNBytes 2)]
   [filename : (Localeof filename-length)]
   [os : (#:enum Byte system->byte byte->system #:fallback 'unused)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require (for-syntax racket/base))

  (define-syntax (call-with-lc stx)
    (syntax-case stx []
      [(_ lc-all sexp ...)
       (syntax/loc stx
         (begin (printf "========= ~a =========~n" lc-all)
                (parameterize ([default-stdin-locale lc-all]
                               [default-stdout-locale lc-all])
                  sexp)
                ...))]))

  (define-syntax (call-with-locales stx)
    (syntax-case stx []
      [(_ [lc-all ...] sexp ...)
       (syntax/loc stx
         (begin (call-with-lc 'lc-all sexp ...)
                ...))]))
  
  (define entry : Stdio-Entry
    (make-stdio-entry #:os 'Darwin #:compression 'stored #:filename "奇葩文件名.docx"
                      #:comment #"test encoded filename to satisfy WinZip"))

  (define eocdir64 : ZIP64-End-Of-Central-Directory
    (make-zip64-end-of-central-directory #:csystem 'Darwin #:cversion 62 #:esystem 'FAT #:eversion 30
                                         #:cdir-offset 0 #:cdir-size 1024 #:cdir-count 1 #:cdir-total 1
                                         #:data (stdio-entry->bytes entry)))

  (default-stdout-all-fields? #false)

  (bytes->zip64-end-of-central-directory (zip64-end-of-central-directory->bytes eocdir64))
  (display-zip64-end-of-central-directory eocdir64)

  (newline)

  (call-with-locales [UTF-8 GB18030]
    (bytes->stdio-entry (stdio-entry->bytes entry))
    (display-stdio-entry entry #:with-offset? #true)))
