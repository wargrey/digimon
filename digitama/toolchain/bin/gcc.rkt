#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/symbol)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-cpp-macros : CC-CPP-Macros
  (lambda [system cpp?]
    (list "-D_POSIX_C_SOURCE=200809L")))

(define gcc-compile-flags : CC-Flags
  (lambda [system cpp? hints]
    (append (list "-c" "-O2" "-fPIC" #;"-Wall" #;"-Wno-unknown-pragmas")
            (cond [(not cpp?) (list "-x" "c" "-std=c17")]
                  [else (list "-x" "c++" "-std=c++17")])
            (case system
              [(macosx) (list "-fno-common")]
              [(illumos) (list "-m64")]
              [else null]))))

(define gcc-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (define system-dirs : (Listof String)
      (case system
        [(macosx) (list "/usr/local/include")]
        [else null]))

    (for/list : (Listof String) ([dir (in-list (append extra-dirs system-dirs))])
      (gcc-search-path "-I" dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-linker-flags : LD-Flags
  (lambda [system cpp? shared-object? hints]
    (append (cond [(not shared-object?) null]
                  [else (case system
                          [(macosx) (list #| MH_BUNDLE file type |# "-bundle")]
                          [else (list "-fPIC" "-shared")])])
            (case system
              [(macosx) (list "-flat_namespace" "-undefined" "suppress")]
              [(illumos) (list "-m64")]
              [else null]))))

(define gcc-linker-libpaths : LD-Libpaths
  (lambda [extra-dirs system cpp?]
    (define system-dirs : (Listof String)
      (case system
        [(macosx) (list "/usr/local/lib")]
        [else null]))
    
    (for/list : (Listof String) ([dir (in-list (append extra-dirs system-dirs))])
      (gcc-search-path "-L" dir))))

(define gcc-linker-libraries : LD-Libraries
  (lambda [links tag system cpp?]
    (if (and (eq? tag '#:framework) (eq? system 'macosx))
        (let ([-fw "-framework"]
              [ls (map symbol->immutable-string links)])
          (cons -fw (add-between ls -fw)))
        (for/list : (Listof String) ([l (in-list links)])
          (string-append "-l" (symbol->immutable-string l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gcc-search-path : (-> String Path-String String)
  (lambda [-option dir]
    (string-append -option
                   (cond [(string? dir) dir]
                         [else (path->string dir)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'gcc '(flags macros includes infile "-o" outfile)
                       #:macros gcc-cpp-macros #:flags gcc-compile-flags #:includes gcc-include-paths)
  
  (c-register-linker 'gcc '(flags libpath libraries infiles "-o" outfile)
                     #:flags gcc-linker-flags #:libpaths gcc-linker-libpaths #:libraries gcc-linker-libraries))
  