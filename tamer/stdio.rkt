#lang typed/racket/base

(require digimon/stdio)

(require "../digitama/bintext/zipinfo.rkt")

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
  
  (define entry : ZIP-File
    (let-values ([(mdate mtime) (zip-entry-modify-datetime (current-seconds))])
      (make-zip-file #:esystem 'FAT #:eversion 20
                     #:name "奇葩文件名.docx" #:gpflag 0 #:compression 'stored #:mdate mdate #:mtime mtime
                     #:metainfo #"stupid WinZip")))

  (define eocdir64 : ZIP64-End-Of-Central-Directory
    (make-zip64-end-of-central-directory #:csystem 'Darwin #:cversion 62 #:esystem 'FAT #:eversion 30
                                         #:cdir-offset 0 #:cdir-size 1024 #:cdir-count 1 #:cdir-total 1
                                         #:data (zip-file->bytes entry)))

  (default-stdout-all-fields? #false)

  (bytes->zip64-end-of-central-directory (zip64-end-of-central-directory->bytes eocdir64))
  (display-zip64-end-of-central-directory eocdir64 #:with-offset? #true)

  (newline)

  (define-values (/dev/bsin /dev/bsout) (make-pipe))
    
  (call-with-locales [UTF-8 GB18030]
    (write-zip-file entry /dev/bsout)
    (pipe-content-length /dev/bsin)
    (read-zip-file /dev/bsin)
    (display-zip-file entry #:with-offset? #true #:hex-offset? #false)
    (newline)))
  