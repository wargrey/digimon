#lang typed/racket/base

(provide (all-defined-out))

(require "cc.rkt")
(require "../toolchain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LD-Options (U 'flags 'libpath 'libraries 'subsystem 'infiles 'outfile))

(define-type LD-Flags (-> Symbol Boolean Boolean (Listof Any) (Listof String)))
(define-type LD-Libpaths (-> (Listof Path) Symbol Boolean (Listof String)))
(define-type LD-Libraries (-> (Listof Symbol) (Option Keyword) Symbol Boolean (Listof String)))
(define-type LD-Subsystem (-> Symbol Boolean (Option Symbol) (Listof String)))

(define-type LD-IO-File-Flag (-> Path-String Symbol Boolean (Listof String)))

(struct ld toolchain
  ([flags : LD-Flags]
   [subsystem : LD-Subsystem]
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

(define ld-default-no-subsystem-flag : LD-Subsystem
  (lambda [system cpp? ?subsystem]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-database : (HashTable Symbol LD) (make-hasheq))

(define c-register-linker : (-> Symbol (Listof (U LD-Options String))
                                #:flags LD-Flags #:libpaths LD-Libpaths #:libraries LD-Libraries
                                [#:subsystem LD-Subsystem] [#:infile LD-IO-File-Flag] [#:outfile LD-IO-File-Flag]
                                [#:basename (Option Symbol)]
                                Void)
  (lambda [name layout
                #:flags flags #:libpaths libpaths #:libraries libraries
                #:subsystem [subsystem ld-default-no-subsystem-flag]
                #:infile [infile ld-default-io-file] #:outfile [outfile ld-default-io-file]
                #:basename [basename #false]]
    (define ld : Symbol (or basename name))
    (define program : (Option Path) (c-find-binary-path ld))
    (define program++ : (Option Path) (c-find-binary-path (c-cpp-partner ld)))

    (when (path? program)
      (hash-set! ld-database name
                 (make-ld program layout
                          flags subsystem libpaths libraries
                          infile outfile
                          (or program++ program))))))
