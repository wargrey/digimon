#lang typed/racket/base

(require racket/path)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/modeline.rkt")

(require "../../system.rkt")

(define msvc-basename : Symbol 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-cpp-macros : CC-CPP-Macros
  (lambda [system cpp?]
    (list "-DF_LAMBDA=__declspec(dllexport)")))

(define msvc-compile-flags : CC-Flags
  (lambda [system cpp?]
    (list "/nologo"
          "/c" ; compiling only, no link
          "/MT" "/O2")))

(define msvc-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (define root+arch : (Option (Pairof Path Path)) (msvc-root+arch))

    (append (map msvc-build-include-path extra-dirs)
            (cond [(not root+arch) null]
                  [else (list* (msvc-build-include-path (car root+arch) "include")
                               (let ([sdkroot (#%info 'msvc-sdk-root)]
                                     [sdk-lib (#%info 'msvc-sdk-library)])
                                 (or (and (path-string? sdkroot)
                                          (path-string? sdk-lib)
                                          (list (msvc-build-include-path sdkroot "Include" sdk-lib "shared")
                                                (msvc-build-include-path sdkroot "Include" sdk-lib "um")
                                                (msvc-build-include-path sdkroot "Include" sdk-lib "ucrt")))
                                     null)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-linker-flags : LD-Flags
  (lambda [system cpp?]
    (list "/nologo" "/LD")))

(define msvc-linker-libpaths : LD-Libpaths
  (lambda [system cpp?]
    (define root+arch : (Option (Pairof Path Path)) (msvc-root+arch))
    (cond [(not root+arch) null]
          [else (let ([arch (cdr root+arch)])
                  (list* (msvc-build-libpath (car root+arch) "lib" arch)
                         (let ([sdkroot (#%info 'msvc-sdk-root)]
                               [sdk-lib (#%info 'msvc-sdk-library)])
                           (or (and (path-string? sdkroot)
                                    (path-string? sdk-lib)
                                    (list (msvc-build-libpath sdkroot "Lib" sdk-lib "um" arch)
                                          (msvc-build-libpath sdkroot "Lib" sdk-lib "ucrt" arch)))
                               null))))])))

(define msvc-linker-libraries : LD-Libraries
  (lambda [modeline system cpp?]
    (define kw : Symbol (or (c:mdl:ld-keyword modeline) system))
    (define ls : (Listof String) (c:mdl:ld-libraries modeline))
    
    (cond [(not (memq kw (list 'static system))) null]
          [else (map (λ [[l : String]] (string-append l ".lib")) ls)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bindir-path : Path (build-path "bin"))

(define msvc-root+arch : (-> (Option (Pairof Path Path)))
  (lambda []
    (define cc : (Option Path) (find-executable-path (format "~a.exe" msvc-basename) #false))
    (and (path? cc)
         (let-values ([(parent arch dir?) (split-path (assert (path-only cc) path?))])
           (and (and (path? parent) (path? arch))
                (let search-root ([dir : Path parent])
                  (let-values ([(parent name dir?) (split-path dir)])
                    (and (path? parent)
                         (cond [(and (path? name) (equal? name bindir-path)) (cons parent arch)]
                               [else (search-root parent)])))))))))

(define msvc-build-include-path : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (string-append "/I"
                   (cond [(pair? subpaths) (path->string (apply build-path subpath subpaths))]
                         [(string? subpath) subpath]
                         [else (path->string subpath)]))))

(define msvc-build-libpath : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (string-append "/LIBPATH:" (path->string (apply build-path subpath subpaths)))))

(define msvc-make-outfile : (-> String LD-IO-File-Flag)
  (lambda [flag]
    (λ [dest system cpp?]
      (list (string-append "/" flag (if (path? dest) (path->string dest) dest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'msvc '(flags macros includes infile outfile) 
                       #:macros msvc-cpp-macros #:flags msvc-compile-flags #:includes msvc-include-paths
                       #:outfile (msvc-make-outfile "Fo")
                       #:basename msvc-basename)
  
  (c-register-linker 'msvc '(flags infiles libraries outfile "/link" libpath)
                     #:flags msvc-linker-flags #:libpaths msvc-linker-libpaths #:libraries msvc-linker-libraries
                     #:outfile (msvc-make-outfile "Fe")
                     #:basename msvc-basename))
  