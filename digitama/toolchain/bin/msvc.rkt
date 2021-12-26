#lang typed/racket/base

(require racket/path)
(require racket/symbol)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/cc.rkt")

(require "../../system.rkt")

(define msvc-basename : Symbol 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-cpp-macros : CC-CPP-Macros
  (lambda [system cpp?]
    null))

(define msvc-compile-flags : CC-Flags
  (lambda [system cpp? hints]
    (append (list "/nologo" "/c" ; compiling only, no link
                  "/O2" #;"/constexpr"
                  "/W3" "/sdl" #;'| security features and warnings |)
            (cond [(not cpp?) (list "/TC" "/std:c17")]
                  [else (list "/TP" "/std:c++17")]))))

(define msvc-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (define root+arch : (Option (Pairof Path Path)) (msvc-root+arch))

    (append (map msvc-build-include-path extra-dirs)
            (cond [(not root+arch) null]
                  [else (list* (msvc-build-include-path (car root+arch) "include")
                               (let ([kitroot (#%info 'msvc-kits-rootdir)]
                                     [version (#%info 'msvc-kits-version)])
                                 (or (and (path-string? kitroot)
                                          (path-string? version)
                                          (let ([incdir (build-path kitroot "Include" version)])
                                            (list (msvc-build-include-path incdir "shared")
                                                  (msvc-build-include-path incdir "um")
                                                  (msvc-build-include-path incdir "ucrt"))))
                                     null)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-linker-flags : LD-Flags
  (lambda [system cpp? dll? hints]
    (list* "/nologo"
           (cond [(not dll?) (list "/MT")]
                 [else (list "/MD" "/LD")]))))

(define msvc-subsystem-flags : LD-Subsystem
  (lambda [system cpp? ?subsystem]
    (cond [(not ?subsystem) null]
          [else (list (format "/SUBSYSTEM:~a"
                        (string-upcase (symbol->immutable-string
                                        ?subsystem))))])))

(define msvc-linker-libpaths : LD-Libpaths
  (lambda [extra-dirs system cpp?]
    (define root+arch : (Option (Pairof Path Path)) (msvc-root+arch))

    (cond [(not root+arch) null]
          [else (let ([arch (cdr root+arch)])
                  (list* (msvc-build-libpath (car root+arch) "lib" arch)
                         (let ([kitroot (#%info 'msvc-kits-rootdir)]
                               [version (#%info 'msvc-kits-version)])
                           (or (and (path-string? kitroot)
                                    (path-string? version)
                                    (let ([libdir (build-path kitroot "Lib" version)])
                                      (list (msvc-build-libpath libdir "um" arch)
                                            (msvc-build-libpath libdir "ucrt" arch))))
                               null))))])))

(define msvc-linker-libraries : LD-Libraries
  (lambda [links tag system cpp?]
    (for/list : (Listof String) ([l (in-list links)])
      (string-append (symbol->immutable-string l) ".lib"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bindir-path : Path (build-path "bin"))

(define msvc-root+arch : (-> (Option (Pairof Path Path)))
  (lambda []
    (define cc : (Option Path) (c-find-binary-path msvc-basename))
    (and (path? cc)
         (let-values ([(parent arch dir?) (split-path (assert (path-only cc) path?))])
           (and (and (path? parent) (path? arch))
                (let search-root ([dir : Path parent])
                  (let-values ([(parent name dir?) (split-path dir)])
                    (and (path? parent)
                         (cond [(and (path? name) (equal? name bindir-path)) (cons parent arch)]
                               [else (search-root parent)])))))))))

(define msvc-search-path : (-> String Path-String (Listof Path-String) String)
  (lambda [/option subpath subpaths]
    (string-append /option
                   (cond [(pair? subpaths) (path->string (apply build-path subpath subpaths))]
                         [(string? subpath) subpath]
                         [else (path->string subpath)]))))

(define msvc-make-outfile : (-> String LD-IO-File-Flag)
  (lambda [flag]
    (λ [dest system cpp?]
      (list (string-append "/" flag (if (path? dest) (path->string dest) dest))))))

(define msvc-build-include-path : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (msvc-search-path "/I" subpath subpaths)))

(define msvc-build-libpath : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (msvc-search-path "/LIBPATH:" subpath subpaths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'msvc '(flags macros includes infile outfile) 
                       #:macros msvc-cpp-macros #:flags msvc-compile-flags #:includes msvc-include-paths
                       #:outfile (msvc-make-outfile "Fo")
                       #:basename msvc-basename)
  
  (c-register-linker 'msvc '(flags infiles libraries outfile "/link" libpath subsystem)
                     #:flags msvc-linker-flags #:subsystem msvc-subsystem-flags
                     #:libpaths msvc-linker-libpaths #:libraries msvc-linker-libraries
                     #:outfile (msvc-make-outfile "Fe")
                     #:basename msvc-basename))
  