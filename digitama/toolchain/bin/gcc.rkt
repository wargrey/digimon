#lang typed/racket/base

(provide (all-defined-out))

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")

(require "clang.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'gcc '(flags macros includes infile "-o" outfile)
                       #:macros clang-cpp-macros #:includes clang-include-paths
                       #:flags clang-compile-flags)
  
  (c-register-linker 'gcc '(flags libpath libraries infiles "-o" outfile)
                     #:libpaths clang-linker-libpaths #:libraries clang-linker-libraries
                     #:flags clang-linker-flags))
