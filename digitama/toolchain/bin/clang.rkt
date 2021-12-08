#lang typed/racket/base

(require racket/list)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/modeline.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clang-cpp-macros : CC-CPP-Macros
  (lambda [system cpp?]
    (list "-DF_LAMBDA="
          "-D_POSIX_C_SOURCE=200809L")))

(define clang-compile-flags : CC-Flags
  (lambda [system cpp?]
    (list* "-c" "-O2" "-fPIC" "-std=c11"
           (case system
             [(macosx) (list "-fno-common")]
             [(illumos) (list "-m64")]
             [else null]))))

(define clang-include-paths : CC-Includes
  (lambda [system cpp?]
    (case system
      [(macosx) (list "-I/usr/local/include")]
      [else null])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clang-linker-flags : LD-Flags
  (lambda [system cpp?]
    (case system
      [(macosx) (list "-bundle" "-flat_namespace" "-undefined" "suppress")]
      [(illumos) (list "-fPIC" "-shared" "-m64")]
      [else (list "-fPIC" "-shared")])))

(define clang-linker-libpaths : LD-Libpaths
  (lambda [system cpp?]
    (case system
      [(macosx) (list "-L/usr/local/lib")]
      [else null])))

(define clang-linker-libraries : LD-Libraries
  (lambda [modeline system cpp?]
    (define kw : Symbol (or (c:mdl:ld-keyword modeline) system))
    (define ls : (Listof String) (c:mdl:ld-libraries modeline))
    (cond [(eq? system kw)
           (map (λ [[l : String]] (string-append "-l" l)) ls)]  ; /* ld: (ssh2) or ld:illumos: (kstat) */
          [(and (eq? system 'macosx) (eq? kw 'framework))       ; /* ld:framework: IOKit */
           (let ([-fw "-framework"])
             (cons -fw (add-between ls -fw)))]
          [else null])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c-register-compiler 'clang '(flags macros includes infile "-o" outfile)
                     #:macros clang-cpp-macros #:flags clang-compile-flags #:includes clang-include-paths)
  
(c-register-linker 'clang '(flags libpath libraries infiles "-o" outfile)
                   #:flags clang-linker-flags #:libpaths clang-linker-libpaths #:libraries clang-linker-libraries)
