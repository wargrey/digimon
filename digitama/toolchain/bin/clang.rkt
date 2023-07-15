#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/symbol)
(require racket/string)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")

(require "../../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clang-cpp-macros : CC-CPP-Macros
  (lambda [default-macros system cpp? extra-macros]
    (map clang-macro->string
         (append default-macros
                 (list (cons "_POSIX_C_SOURCE" "200809L"))
                 extra-macros))))

(define clang-compile-flags : CC-Flags
  (lambda [system cpp? hints verbose? debug?]
    (append (list "-c" "-fPIC" "-Wall")
            (if (not debug?) (list "-O2") (list "-Og" "-g"))
            (cond [(not cpp?) (list "-x" "c" "-std=c17")]
                  [else (list "-x" "c++" "-std=c++17")])
            (case system
              [(macosx) (list "-fno-common")]
              [(illumos) (list "-m64")]
              [else null])
            (list "-fcolor-diagnostics" "-fansi-escape-codes")
            (if (not verbose?) null (list "-v")))))

(define clang-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (define system-dirs : (Listof String)
      (case system
        [(macosx) (list "/usr/local/include")]
        [else null]))

    (for/list : (Listof String) ([dir (in-list (append extra-dirs system-dirs))])
      (clang-search-path "-I" dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clang-linker-flags : LD-Flags
  (lambda [system cpp? shared-object? hints verbose? pass-to-linker?]
    (append (cond [(not shared-object?) null]
                  [else (case system
                          ;;; WARNING
                          ; the key difference between the MH_DYLIB and MH_BUNDLE file types is that
                          ;   MH_BUNDLE files must be `dlopen`ed  at run time
                          ;   instead of being linked at compile time.
                          [(macosx) (list #| MH_DYLIB file type |# "-dynamiclib")]
                          [else (list "-fPIC" "-shared")])])
            (if (not cpp?) (list "-std=c17") (list "-std=c++17"))
            (case system
              [(macosx) (list "-flat_namespace" "-undefined" "suppress")]
              [(illumos) (list "-m64")]
              [else null])
            (if (not verbose?) null (list "-v")))))

(define clang-linker-libpaths : LD-Libpaths
  (lambda [extra-dirs system cpp?]
    (define system-dirs : (Listof String)
      (case system
        [(macosx) (list "/usr/local/lib")]
        [else null]))
    
    (for/list : (Listof String) ([dir (in-list (append extra-dirs system-dirs))])
      (clang-search-path "-L" dir))))

(define clang-linker-libraries : LD-Libraries
  (lambda [links tag system cpp?]
    (if (and (eq? tag '#:framework) (eq? system 'macosx))
        (let ([-fw "-framework"]
              [ls (map symbol->immutable-string links)])
          (cons -fw (add-between ls -fw)))
        (for/list : (Listof String) ([l (in-list links)])
          (string-append "-l" (symbol->immutable-string l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clang-macro->string : (-> (Pairof String (Option String)) String)
  (lambda [macro]
    (define-values (name body) (values (car macro) (cdr macro)))

    (cond [(string-contains? name "(") (raise-user-error 'cc "function-like macro is not allowed: ~a." name)]
          [(not body) (string-append "-D" name)]
          [(string-contains? body " ") (raise-user-error 'cc "whitespace is not allowed here: '~a'." body)]
          [else (string-append "-D" name "=" body)])))

(define clang-search-path : (-> String Path-String String)
  (lambda [-option dir]
    (string-append -option (path->string/quote dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'clang '(flags macros includes infile "-o" outfile)
                       #:macros clang-cpp-macros #:includes clang-include-paths
                       #:flags clang-compile-flags)
  
  (c-register-linker 'clang '(flags libpath libraries infiles "-o" outfile)
                     #:libpaths clang-linker-libpaths #:libraries clang-linker-libraries
                     #:flags clang-linker-flags))
