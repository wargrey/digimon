#lang typed/racket/base

(provide (all-defined-out))

(require "cc.rkt")
(require "modeline.rkt")

(define-type LD-Options (U 'flags 'libpath 'libraries 'infiles 'outfile))

(define-type LD-Flags (-> Symbol (Listof String)))
(define-type LD-Libpaths (-> Symbol (Listof String)))
(define-type LD-Libraries (-> (Listof C-LD-Modeline) Symbol (Listof String)))

(define-type LD-IO-File-Flag (-> Path-String Symbol (Listof String)))

(struct ld c-toolchain
  ([flags : LD-Flags]
   [libpaths : LD-Libpaths]
   [libraries : LD-Libraries]
   [infile : LD-IO-File-Flag]
   [outfile : LD-IO-File-Flag])
  #:constructor-name make-ld
  #:type-name LD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-default-io-file : LD-IO-File-Flag
  (lambda [dest system]
    (list (if (path? dest) (path->string dest) dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-database : (HashTable Symbol LD) (make-hasheq))

(define c-register-linker : (-> Symbol (Listof (U LD-Options String))
                                #:flags LD-Flags #:libpaths LD-Libpaths #:libraries LD-Libraries
                                [#:infile LD-IO-File-Flag] [#:outfile LD-IO-File-Flag]
                                [#:basename (Option Symbol)]
                                Void)
  (lambda [name layout
                #:flags flags #:libpaths libpaths #:libraries libraries
                #:infile [infile ld-default-io-file] #:outfile [outfile ld-default-io-file]
                #:basename [basename #false]]
    (define program : (Option Path) (c-find-binary-path (or basename name)))

    (when (path? program)
      (hash-set! ld-database name
                 (make-ld program layout
                          flags libpaths libraries
                          infile outfile)))))
