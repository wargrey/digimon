#lang typed/racket/base

(require racket/list)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")

(require "gcc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'clang '(flags macros includes infile "-o" outfile)
                       #:macros gcc-cpp-macros #:flags gcc-compile-flags #:includes gcc-include-paths)
  
  (c-register-linker 'clang '(flags libpath libraries infiles "-o" outfile)
                     #:flags gcc-linker-flags #:libpaths gcc-linker-libpaths #:libraries gcc-linker-libraries))
