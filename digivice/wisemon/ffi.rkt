#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require "path.rkt")
(require "spec.rkt")
(require "parameter.rkt")
(require "native.rkt")

(require "../../digitama/system.rkt")
(require "../../digitama/toolchain/cc/configuration.rkt")

;;; NOTE
;     It is supposed that:
;       1. header files are rarely used for FFI implementations
;       2. internal header files of common functions are placed beside their source files
;       3. a header file only has one implementing source file
;
;     But, even if those assumptions are not true, foreign code would also work just fine
;       as long as the dependent symbols of shared objects are manually loaded into the process
;       by `require`ing corresponding racket files that define the FFI symbols.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define px.so : Regexp (pregexp (format "[.]~a$" (subbytes (system-type 'so-suffix) 1))))

(define make-ffi-library-specs : (->* (Info-Ref) ((Listof Path)) Wisemon-Specification)
  (lambda [info-ref [ex-shared-objects null]]
    (append (make-c-library-specs info-ref #px"\\.c$" #false ex-shared-objects)
            (make-c-library-specs info-ref #px"\\.cpp$" #true ex-shared-objects))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-c-library-specs : (-> Info-Ref Regexp Boolean (Listof Path) Wisemon-Specification)
  (lambda [info-ref px.ext cpp? ex-shared-objects]
    (define rootdir (path->string (digimon-path 'zone)))
    (define configs (#%info 'ffi-toolchain-config))
    (define compilers (ffi-compiler-filter #%info))
    (define standard (c-language-standard-filter #%info (if cpp? 'ffi-cpp-standard 'ffi-c-standard)))
    (define-values (macros includes libpaths libraries) (c-configuration-filter (if (list? configs) configs null) digimon-system))

    (for/fold ([specs : Wisemon-Specification null])
              ([c (in-list (find-digimon-files (λ [[file : Path]] (regexp-match? px.ext file)) (current-directory)))])
      (define contained-in-package?  : Boolean (digimon-stone-path? c))
      (define ffi? : Boolean (file-exists? (path-replace-extension c #".rkt")))
      (define c.o : Path (assert (c-source->object-file c)))
      
      (list* (wisemon-spec c.o #:^ (cons c (c-include-headers c #:topic (current-make-phony-goal)))
                           #:- (c-compile #:standard standard #:cpp? cpp? #:compilers compilers
                                          #:includes (cons rootdir includes)
                                          #:macros (cons '__racket__ macros)
                                          #:verbose? (make-trace-log)
                                          c c.o))

             (cond [(or contained-in-package? (and ffi? (not (member c ex-shared-objects))))
                    (let* ([c.so (assert (c-source->shared-object-file c contained-in-package? #:lib-prefixed? #false) path?)]
                           [objects.h (c-include-headers c #:check-source? #true #:topic (current-make-phony-goal))]
                           [objects (cons c.o (c-headers->files objects.h c-source->object-file))])
                      (cons (wisemon-spec c.so #:^ objects
                                          #:- (c-link #:linkers compilers #:cpp? cpp?
                                                      #:subsystem #false #:entry #false
                                                      #:libpaths libpaths #:libraries libraries
                                                      #:verbose? (make-trace-log)
                                                      objects c.so))
                            specs))]
                   [else specs])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ffi-compiler-filter : (-> Info-Ref (Listof Symbol))
  (lambda [info-ref]
    (c-compiler-filter info-ref "~a-toolchain-names" 'ffi)))
