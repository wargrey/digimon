#lang typed/racket/base

(provide (all-defined-out))

(require racket/promise)

(require "cc.rkt")
(require "../toolchain.rkt")
(require "../../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type LD-Options (U 'flags 'libpath 'libraries 'subsystem 'infiles 'outfile 'ldflags))

(define-type LD-Flags (-> Symbol Boolean Boolean Boolean Boolean (Listof String)))
(define-type LD-Libpaths (-> (Listof Path) Symbol Boolean (Listof String)))
(define-type LD-Libraries (-> (Listof Symbol) (Option Keyword) Symbol Boolean (Listof String)))
(define-type LD-Subsystem (-> Symbol Boolean (Option Symbol) (Option Keyword) (Listof String)))

(define-type LD-IO-File-Flag (-> Path-String Symbol Boolean (Listof String)))

(struct ld toolchain
  ([flags : LD-Flags]
   [subsystem : LD-Subsystem]
   [libpaths : LD-Libpaths]
   [libraries : LD-Libraries]
   [infile : LD-IO-File-Flag]
   [outfile : LD-IO-File-Flag]
   [++ : (Option (Promise (Option Path)))])
  #:constructor-name make-ld
  #:type-name LD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-default-io-file : LD-IO-File-Flag
  (lambda [dest system cpp?]
    (list (path->string/quote (find-relative-path (current-directory) dest)))))

(define ld-default-no-subsystem-flag : LD-Subsystem
  (lambda [system cpp? ?subsystem ?entry]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ld-database : (HashTable Symbol LD) (make-hasheq))

(define c-register-linker : (-> Symbol (Listof (U LD-Options String))
                                #:flags LD-Flags #:libpaths LD-Libpaths #:libraries LD-Libraries
                                [#:subsystem LD-Subsystem] [#:infile LD-IO-File-Flag] [#:outfile LD-IO-File-Flag]
                                [#:basename (Option Symbol)] [#:find-linker (-> Symbol Symbol (Option Path))]
                                [#:env (U False Environment-Variables (-> Environment-Variables))]
                                Void)
  (lambda [name layout
                #:flags flags #:libpaths libpaths #:libraries libraries
                #:subsystem [subsystem ld-default-no-subsystem-flag]
                #:infile [infile ld-default-io-file] #:outfile [outfile ld-default-io-file]
                #:basename [basename #false] #:find-linker [find-linker c-find-binary-path] #:env [env #false]]
    (define ld : Symbol (or basename name))
    (define ld++ : Symbol (c-cpp-partner ld))
    
    (hash-set! ld-database name
               (make-ld (lazy (find-linker ld 'ld))
                        layout env
                        flags subsystem libpaths libraries
                        infile outfile
                        (and (not (eq? ld ld++))
                             (lazy (find-linker ld++ 'ld)))))))
