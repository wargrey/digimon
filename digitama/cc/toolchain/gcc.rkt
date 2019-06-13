#lang typed/racket/base

(require "../compiler.rkt")

(define gcc-cpp-macros : CC-CPP-Macros
  (lambda [system]
    (list "-DF_LAMBDA="
          "-D_POSIX_C_SOURCE=200809L")))

(define gcc-compile-flags : CC-Flags
  (lambda [system]
    (list* "-c" "-O2" "-fPIC" "-std=c11"
           (case system
             [(macosx) (list "-fno-common")]
             [(illumos) (list "-m64")]
             [else null]))))

(define gcc-include-paths : CC-Includes
  (lambda [system]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (eq? (system-type 'os) 'windows)
  (c-register-compiler 'gcc
                       (make-cc 'gcc
                                '(flags macros includes infile outfile)
                                gcc-cpp-macros
                                gcc-compile-flags
                                gcc-include-paths)))
