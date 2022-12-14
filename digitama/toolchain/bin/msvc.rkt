#lang typed/racket/base

(require racket/string)
(require racket/symbol)
(require racket/keyword)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/cc.rkt")

(require "../../exec.rkt")
(require "../../system.rkt")
(require "../../../filesystem.rkt")

(define msvc-basename : Symbol 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-cpp-macros : CC-CPP-Macros
  (lambda [default-macros system cpp? extra-macros]
    (map msvc-macro->string
         (append default-macros
                 extra-macros))))

(define msvc-compile-flags : CC-Flags
  (lambda [system cpp? hints verbose?]
    (append (list "/nologo" "/c" ; compiling only, no link
                  "/O2" #;"/constexpr"
                  "/EHsc" "/W3" "/sdl" #;'| security features and warnings |)
            (if (not cpp?) (list "/TC" "/std:c17") (list "/TP" "/std:c++17"))
            (if (not verbose?) null (list "/showIncludes")))))

(define msvc-include-paths : CC-Includes
  (lambda [extra-dirs system cpp?]
    (append (map msvc-build-incpath extra-dirs)
            #;(let ([root+arch (msvc-root+arch)])
                (cond [(not root+arch) null]
                      [else (list* (msvc-build-include-path (car root+arch) "include")
                                   (let ([incdir (msvc-windows-kit-rootdir "Include")])
                                     (or (and incdir (directory-exists? incdir)
                                              (list (msvc-build-include-path incdir "ucrt")
                                                    (msvc-build-include-path incdir "um")
                                                    (msvc-build-include-path incdir "shared")))
                                         null)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-linker-flags : LD-Flags
  (lambda [system cpp? dll? hints verbose? pass-to-linker?]
    (if (not pass-to-linker?)
        (append (list "/nologo")
                (if (not dll?) (list "/MT") (list "/MD" "/LD")))
        (append (if (not verbose?) null (list "/VERBOSE"))))))

(define msvc-subsystem-flags : LD-Subsystem
  (lambda [system cpp? ?subsystem ?entry]
    (filter string?
            (list (and ?subsystem (format "/SUBSYSTEM:~a" (string-upcase (symbol->immutable-string ?subsystem))))
                  (and ?entry (format "/ENTRY:~a" (keyword->immutable-string ?entry)))))))
  
(define msvc-linker-libpaths : LD-Libpaths
  (lambda [extra-dirs system cpp?]
    (append (map msvc-build-libpath extra-dirs)
            #;(let ([root+arch (msvc-root+arch)])
              (cond [(not root+arch) null]
                    [else (let ([arch (cdr root+arch)])
                            (list* (msvc-build-libpath (car root+arch) "lib" arch)
                                   (let ([libdir (msvc-windows-kit-rootdir "Lib")])
                                     (or (and libdir (directory-exists? libdir)
                                              (list (msvc-build-libpath libdir "um" arch)
                                                    (msvc-build-libpath libdir "ucrt" arch)))
                                         null))))])))))

(define msvc-linker-libraries : LD-Libraries
  (lambda [links tag system cpp?]
    (for/list : (Listof String) ([l (in-list links)])
      (string-append (symbol->immutable-string l) ".lib"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define bindir-path : Path (build-path "bin"))

(define msvc-macro->string : (-> (Pairof String (Option String)) String)
  (lambda [macro]
    (define-values (name body) (values (car macro) (cdr macro)))

    (cond [(not body) (string-append "/D" name)]
          [(or (string-contains? name "(") (string-contains? body " ")) (string-append "/D\"" name "=" body "\"")]
          [else (string-append "/D" name "=" body)])))

#;(define msvc-root+arch : (-> (Option (Pairof Path Path)))
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

#;(define msvc-windows-kit-rootdir : (-> String (Option Path))
  (lambda [type]
    (define kitroot (#%info 'msvc-kits-rootdir))
    (define version (#%info 'msvc-kits-version))

    (for/or : (Option Path) ([kitroot : Any (if (list? kitroot) (in-list kitroot) (in-value kitroot))])
      (for/or : (Option Path) ([version : Any (if (list? version) (in-list version) (in-value version))])
        (and (path-string? kitroot) (path-string? version)
             (build-path kitroot type version))))))

(define msvc-search-path : (-> String Path-String (Listof Path-String) String)
  (lambda [/option subpath subpaths]
    (string-append /option
                   (cond [(pair? subpaths) (path->string/quote (apply build-path subpath subpaths))]
                         [else (path->string/quote subpath)]))))

(define msvc-make-outfile : (-> String LD-IO-File-Flag)
  (lambda [flag]
    (λ [dest system cpp?]
      (list (string-append "/" flag (path->string/quote dest))))))

(define msvc-build-incpath : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (msvc-search-path "/I" subpath subpaths)))

(define msvc-build-libpath : (-> Path-String Path-String * String)
  (lambda [subpath . subpaths]
    (msvc-search-path "/LIBPATH:" subpath subpaths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-path : (-> Symbol (Option Path))
  (let ([&env : (Boxof (Option Environment-Variables)) (box #false)])
    (lambda [basename]
      (or (c-find-binary-path basename)
          (let ([vcvarsall.bat (msvc-vcvarsall-path #false)])
            (and vcvarsall.bat
                 (parameterize ([current-environment-variables (msvc-make-envs vcvarsall.bat &env)])
                   (c-find-binary-path basename))))))))

(define msvc-vcvarsall-path : (case-> [True -> Path]
                                      [Boolean -> (Option Path)])
  (let ([vars.bat : String "vcvarsall.bat"]
        [info-var : Symbol 'msvc-devcmd-dir])
    (lambda [required?]
      (define devcmd-dir (#%info info-var))
      
      (or (for/or : (Option Path) ([devcmd : Any (if (list? devcmd-dir) (in-list devcmd-dir) (in-value devcmd-dir))])
            (and (path-string? devcmd)
                 (let ([bat (build-path devcmd vars.bat)])
                   (and (file-exists? bat) bat))))
          (find-executable-path vars.bat)
          (and required?
               (raise-user-error 'msvc-vcvarsall-path
                                 (string-append "Microsoft makes it really annoying to work with the tool chain from commandline.\n"
                                                (format "I need the `~a` to set environment variables to satisfy MSVC.\n" vars.bat)
                                                "Please configure it by either adding its path to PATH, "
                                                (format "or defining it as `~a` in the `info.rkt`" info-var))))))))


(define msvc-make-envs : (-> Path (Boxof (Option Environment-Variables)) Environment-Variables)
  (let* ([env.rktl : String "msvc-env.rktl"]
         [rx:rktl : Regexp (regexp (string-append (regexp-quote env.rktl) "$"))])
    (lambda [vcvarsall.bat &env]
      (define msvc-env : Environment-Variables (make-environment-variables))
      (define /dev/envout : Output-Port (open-output-bytes '/dev/envout))
        
      (fg-recon-exec 'vcvarsall (assert (find-executable-path "cmd.exe")) null
                     #:/dev/stdout /dev/envout
                     #:silent '(stdout)
                     #:feeds (list (format "~a x64" (path->string/quote vcvarsall.bat))
                                   (format "~a ~a"
                                     (path->string/quote (or (find-executable-path "racket")
                                                             (find-system-path 'exec-file)))
                                     (path->string/quote (collection-file-path env.rktl "digimon" "stone" "digivice"))))
                     #:feed-eof "exit")
      
      (let ([/dev/envin (open-input-bytes (get-output-bytes /dev/envout) '/dev/envin)])
        (let filter-out ()
          (define line (read-line /dev/envin))
          (when (and (string? line) (not (regexp-match? rx:rktl line)))
            (filter-out)))
        
        (let load-env ()
          (define var (read-line /dev/envin))
          (define val (read-line /dev/envin))
          
          (when (and (string? var) (string? val))
            (environment-variables-set! msvc-env (string->bytes/utf-8 var) (string->bytes/utf-8 val))
            (load-env)))
        
        (unless (not &env)
          (set-box! &env msvc-env))
        msvc-env))))

(define msvc-environment-variables : (-> Environment-Variables)
  (let* ([&msvc-env : (Boxof (Option Environment-Variables)) (box #false)])
    (lambda []
      (or (unbox &msvc-env)
          (msvc-make-envs (msvc-vcvarsall-path #true) &msvc-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'msvc '(flags macros includes infile outfile) 
                       #:macros msvc-cpp-macros #:flags msvc-compile-flags #:includes msvc-include-paths
                       #:outfile (msvc-make-outfile "Fo")
                       #:basename msvc-basename #:find-compiler msvc-path
                       #:env msvc-environment-variables)
  
  (c-register-linker 'msvc '(flags infiles libraries outfile "/link" libpath subsystem ldflags)
                     #:flags msvc-linker-flags #:subsystem msvc-subsystem-flags
                     #:libpaths msvc-linker-libpaths #:libraries msvc-linker-libraries
                     #:outfile (msvc-make-outfile "Fe")
                     #:basename msvc-basename #:find-linker msvc-path
                     #:env msvc-environment-variables))
  