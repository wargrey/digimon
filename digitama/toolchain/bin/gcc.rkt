#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/modeline.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-cpp-macros : CC-CPP-Macros
  (lambda [system cpp?]
    (list "-DF_LAMBDA="
          "-D_POSIX_C_SOURCE=200809L")))

(define gcc-compile-flags : CC-Flags
  (lambda [system cpp?]
    (append (list "-c" "-O2" "-fPIC")
            (cond [(not cpp?) (list "-std=c17")]
                  [else (list "-std=c++17")])
            (case system
              [(macosx) (list "-fno-common")]
              [(illumos) (list "-m64")]
              [else null]))))

(define gcc-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (map gcc-include-path
         (append extra-dirs
                 (case system
                   [(macosx) (list "/usr/local/include")]
                   [else null])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-linker-flags : LD-Flags
  (lambda [system cpp?]
    (case system
      [(macosx) (list "-bundle" "-flat_namespace" "-undefined" "suppress")]
      [(illumos) (list "-fPIC" "-shared" "-m64")]
      [else (list "-fPIC" "-shared")])))

(define gcc-linker-libpaths : LD-Libpaths
  (lambda [system cpp?]
    (case system
      [(macosx) (list "-L/usr/local/lib")]
      [else null])))

(define gcc-linker-libraries : LD-Libraries
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
(define gcc-include-path : (-> Path-String String)
  (lambda [dir]
    (string-append "-I"
                   (cond [(string? dir) dir]
                         [else (path->string dir)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'gcc '(flags macros includes infile "-o" outfile)
                       #:macros gcc-cpp-macros #:flags gcc-compile-flags #:includes gcc-include-paths)
  
  (c-register-linker 'gcc '(flags libpath libraries infiles "-o" outfile)
                     #:flags gcc-linker-flags #:libpaths gcc-linker-libpaths #:libraries gcc-linker-libraries))
  