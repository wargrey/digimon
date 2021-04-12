#lang typed/racket/base

(require digimon/stdio)

(require "../digitama/bintext/zipinfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct stdio-entry : Stdio-Entry
  ([signature : MUInt32 #:omittable-signature #%zip-?data]
   [compression : (#:enum LUInt16 compression-method->index index->compression-method)]
   [filename-length : LUInt16]
   [comment : (MNBytes 2)]
   [filename : (Stringof filename-length)]
   [os : (#:enum Byte system->byte byte->system #:fallback 'unused)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define-values (stdin stdout) (make-pipe))
  (define entry (make-stdio-entry #:os 'Macintosh #:compression 'stored #:filename "二进制文件.zip" #:comment #"read and write binary files"))

  (let ([_ (thread (λ []
                     (write-stdio-entry entry stdout)
                     (close-output-port stdout)))])
    (read-stdio-entry stdin)))
