#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require typed/setup/getinfo)

(require "path.rkt")
(require "spec.rkt")

(require "../../cc.rkt")
(require "../../digitama/system.rkt")

;;; NOTE
;     It is supposed that:
;       1. header files are rarely used for FFI implementations
;       2. internal header files of common functions are placed beside their source files
;       3. a header file only has one implementing source file
;
;     But, even if those assumptions are not true, foreign code would also work just fine
;       as long as the dependent symbols of a shared object are manually loaded into the process
;       by `require`ing corresponding racket files that define the FFI symbols.
;
;     Some modeline rules in header files should be set to drop those assumptions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-native-library-specs : (->* (Info-Ref) ((Listof Path)) Wisemon-Specification)
  (lambda [info-ref [ex-shared-objects null]]
    (append (make-c-library-specs info-ref #px"\\.c$" #false ex-shared-objects)
            (make-c-library-specs info-ref #px"\\.cpp$" #true ex-shared-objects))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-c-library-specs : (-> Info-Ref Regexp Boolean (Listof Path) Wisemon-Specification)
  (lambda [info-ref px.ext cpp? ex-shared-objects]
    (define rootdir (path->string (digimon-path 'zone)))
    (define stone-dir (path->string (digimon-path 'stone)))

    (for/fold ([specs : Wisemon-Specification null])
              ([c (in-list (find-digimon-files (λ [[file : Path]] (regexp-match? px.ext file)) (current-directory)))])
      (define contained-in-package?  : Boolean (string-prefix? (path->string c) stone-dir))
      (define ffi? : Boolean (file-exists? (path-replace-extension c #".rkt")))
      (define deps.h : (Listof Path) (c-include-headers c))
      (define c.o : Path (assert (c-source->object-file c)))
      
      (list* (wisemon-spec c.o #:^ (cons c deps.h) #:- (c-compile c c.o #:cpp? cpp? #:include-dirs (list rootdir)))

             (cond [(or contained-in-package? (and ffi? (not (member c ex-shared-objects))))
                    (let ([objects (cons c.o (c-headers->files deps.h c-source->object-file))]
                          [c.so (assert (c-source->shared-object-file c contained-in-package?) path?)])
                      (cons (wisemon-spec c.so #:^ objects #:- (c-link objects c.so #:cpp? cpp? #:modelines (c-source-modelines c)))
                            specs))]
                   [else specs])))))
