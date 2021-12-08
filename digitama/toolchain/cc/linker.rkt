#lang typed/racket/base

(provide (all-defined-out))

(require "cc.rkt")
(require "modeline.rkt")
(require "../toolchain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LD-Options (U 'flags 'libpath 'libraries 'infiles 'outfile))

(define-type LD-Flags (-> Symbol Boolean (Listof String)))
(define-type LD-Libpaths (-> Symbol Boolean (Listof String)))
(define-type LD-Libraries (-> C-LD-Modeline Symbol Boolean (Listof String)))

(define-type LD-IO-File-Flag (-> Path-String Symbol Boolean (Listof String)))

(struct ld toolchain
  ([flags : LD-Flags]
   [libpaths : LD-Libpaths]
   [libraries : LD-Libraries]
   [infile : LD-IO-File-Flag]
   [outfile : LD-IO-File-Flag]
   [++ : Path])
  #:constructor-name make-ld
  #:type-name LD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-default-io-file : LD-IO-File-Flag
  (lambda [dest system cpp?]
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
    (define ld : Symbol (or basename name))
    (define program : (Option Path) (c-find-binary-path ld))
    (define program++ : (Option Path) (c-find-binary-path (c-cpp-partner ld)))

    (when (path? program)
      (hash-set! ld-database name
                 (make-ld program layout
                          flags libpaths libraries
                          infile outfile
                          (or program++ program))))))
