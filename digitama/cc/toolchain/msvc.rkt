#lang typed/racket/base

(require "../compiler.rkt")
(require "../linker.rkt")

(define msvc-cpp-macros : CC-CPP-Macros
  (lambda [system]
    (list "-DF_LAMBDA=__declspec(dllexport)")))

(define msvc-compile-flags : CC-Flags
  (lambda [system]
    (list "/nologo" "/c" "/MT" "/O2")))

(define msvc-include-paths : CC-Includes
  (lambda [system]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq? (system-type 'os) 'windows)
  (c-register-compiler
   'msvc
   (make-cc 'cl
            '(flags macros includes infile outfile)
            msvc-cpp-macros
            msvc-compile-flags
            msvc-include-paths))
  
  (c-register-linker
   'msvc
   (make-ld 'cl '(flags libraries infiles outfile libpath))))
