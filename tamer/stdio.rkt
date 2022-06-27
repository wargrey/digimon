#lang typed/racket/base

(require digimon/stdio)

(require "../digitama/bintext/zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct stdio-entry : Stdio-Entry
  ([signature : MUInt32 #:signature #%zip-?data #:omittable]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [filename-length : LUInt16]
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
  
  (define entry (make-stdio-entry #:os 'Macintosh #:compression 'stored #:filename "奇葩文件名.docx"
                                  #:comment #"A test case for encoding filename when writing into a zip archive"))

  (default-stdout-all-fields? #false)

  entry
  
  (call-with-locales [UTF-8 GB18030]
    (bytes->stdio-entry (stdio-entry->bytes entry))
    (display-stdio-entry entry #:with-offset? #true)))
