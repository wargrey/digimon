#lang typed/racket/base

(provide (all-defined-out))

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")

(require "clang.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-compile-flags : CC-Flags
  (lambda [system cpp? std verbose? debug?]
    (cond [(eq? system 'macosx) (clang-compile-flags system cpp? std verbose? debug?)]
          [else (append (list "-c" "-fPIC" "-Wall")

                        ; doesn't work...
                        (list "-finput-charset=UTF-8" "-fextended-identifiers" "-fexec-charset=UTF-8")

                        (if (not debug?) (list "-O2") (list "-Og" "-g"))
                        (cond [(not cpp?) (list "-x" "c" (clang-stdc->string std))]
                              [else (list "-x" "c++" (clang-stdcpp->string std))])
                        (case system
                          [(illumos) (list "-m64")]
                          [else null])
                        (list "-fdiagnostics-color=always")
                        (if (not verbose?) null (list "-v")))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'gcc '(flags macros includes infile "-o" outfile)
                       #:macros clang-cpp-macros #:includes clang-include-paths
                       #:flags gcc-compile-flags)
  
  (c-register-linker 'gcc '(flags libpath libraries infiles "-o" outfile)
                     #:libpaths clang-linker-libpaths #:libraries clang-linker-libraries
                     #:flags clang-linker-flags))
