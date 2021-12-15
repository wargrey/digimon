#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/list)

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
    (define cs : (Listof Path) (find-digimon-files (λ [[file : Path]] (regexp-match? px.ext file)) (current-directory)))
    (cond [(null? cs) null]
          [else (let ([rootdir (path->string (digimon-path 'zone))]
                      [stone-dir (path->string (digimon-path 'stone))])
                  (for/fold ([specs : Wisemon-Specification null])
                            ([c (in-list cs)])
                    (define contained-in-package?  : Boolean (string-prefix? (path->string c) stone-dir))
                    (define deps.h : (Listof Path) (c-include-headers c))
                    (define c.o : Path (assert (c-source->object-file c) path?))

                    (list* (wisemon-spec c.o #:^ (cons c deps.h) #:- (c-compile c c.o #:cpp? cpp? #:include-dirs (list rootdir)))

                           (cond [(member c ex-shared-objects) specs] ; those goals are probably executable ones
                                 [else (let ([objects (cons c.o (c-headers->shared-objects deps.h))]
                                             [c.so (assert (c-source->shared-object-file c contained-in-package?) path?)])
                                         (cons (wisemon-spec c.so #:^ objects #:- (c-link objects c.so #:cpp? cpp? #:modelines (c-source-modelines c)))
                                               specs))]))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-header->maybe-source : (-> Path-String (Option Path))
  (lambda [h]
    (for/or : (Option Path) ([ext (in-list (list #".c" #".cpp"))])
      (define h.c (path-replace-extension h ext))
      
      (and (file-exists? h.c)
           (c-source->object-file h.c)))))

(define c-headers->shared-objects : (-> (Listof Path) (Listof Path))
  (lambda [deps]
    (remove-duplicates (filter-map c-header->maybe-source deps))))
