#lang typed/racket/base

(require racket/string)

(require "../cc/compiler.rkt")
(require "../cc/linker.rkt")
(require "../cc/cc.rkt")

(require "../../exec.rkt")
(require "../../system.rkt")
(require "../../../symbol.rkt")
(require "../../../filesystem.rkt")
(require "../../../environ.rkt")
(require "../../../dtrace.rkt")

(define msvc-basename : Symbol 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msvc-cpp-macros : CC-CPP-Macros
  (lambda [default-macros system cpp? extra-macros]
    (map msvc-macro->string
         (append default-macros
                 extra-macros))))

(define msvc-compile-flags : CC-Flags
  (lambda [system cpp? hints verbose? debug?]
    (append (list "/nologo" "/FC" "/c" ; compiling only, no link
                  #;"/constexpr"
                  "/EHsc" "/W3" "/sdl" ; security features and warnings
                  "/utf-8")
            (if (not debug?) (list "/O2") (list "/Od" "/ZI" "/JMC"))
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
        (append (list "/nologo" "/utf-8")
                (if (not dll?) (list "/MT") (list "/MD" "/LD")))
        (append (if (not verbose?) null (list #;"/VERBOSE"))))))

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

    (cond [(string-contains? name "(") (raise-user-error 'cc "function-like macro is not allowed: ~a." name)]
          [(not body) (string-append "/D" name)]
          [(string-contains? body " ") (string-append "/D\"" name "=" body "\"")]
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
(define &msvc-env : (Boxof (Option Environment-Variables)) (box #false))

(define msvc-path : (-> Symbol Symbol (Option Path))
  (lambda [basename topic]
    (define msvc-env (unbox &msvc-env))

    (if (not msvc-env)
        (let ([epath (c-find-binary-path basename topic)])
          (if (not epath)
              (let-values ([(full-vcvarsall.bat vcvarsall.bat) (msvc-vcvarsall-path #false topic)])
                (and full-vcvarsall.bat
                     (parameterize ([current-environment-variables (msvc-make-envs full-vcvarsall.bat vcvarsall.bat &msvc-env)])
                       (c-find-binary-path basename topic))))
              (begin (set-box! &msvc-env (current-environment-variables))
                     epath)))
        (parameterize ([current-environment-variables msvc-env])
          (c-find-binary-path basename topic)))))

(define msvc-vcvarsall-path : (case-> [True Symbol -> (Values Path String)]
                                      [Boolean Symbol -> (Values (Option Path) String)])
  (let* ([vars.bat : String "vcvarsall.bat"]
         [info-var : Symbol 'msvc-devcmd-dir]
         [log-msg : String (string-append "Microsoft makes it really annoying to work with the tool chain from commandline.\n"
                                          (format "I need the `~a` to set environment variables to satisfy MSVC.\n" vars.bat)
                                          "Please configure it by either adding its path to PATH, "
                                          (format "or defining it as `~a` in the `info.rkt`" info-var))])
    (lambda [required? topic]
      (define devcmd-dir (#%info info-var))
      
      (values
       (or (for/or : (Option Path) ([devcmd : Any (if (list? devcmd-dir) (in-list devcmd-dir) (in-value devcmd-dir))])
             (and (path-string? devcmd)
                  (let ([bat (build-path devcmd vars.bat)])
                    (and (file-exists? bat) bat))))
           (find-executable-path vars.bat)
           (if required?
               (raise-user-error 'msvc-vcvarsall-path log-msg)
               (begin (dtrace-debug #:topic topic log-msg) #false)))
       vars.bat))))

(define msvc-make-envs : (-> Path String (Boxof (Option Environment-Variables)) Environment-Variables)
  (lambda [full-vcvarsall.bat vcvarsall.bat &env]
    (define msvc-env : Environment-Variables (environment-variables-copy (current-environment-variables)))
    (define /dev/envout : Output-Port (open-output-bytes '/dev/envout))
    (define /dev/envin : Input-Port (open-input-string (format "~a x64~n SET~n exit~n" (path->string/quote full-vcvarsall.bat))))
    
    (fg-recon-exec 'vcvarsall (assert (find-executable-path "cmd.exe")) null
                   #:silent '(stdout) #:/dev/stdout /dev/envout
                   #:stdin-log-level 'info
                   #:/dev/stdin /dev/envin)
    
    (let ([/dev/envin (open-input-bytes (get-output-bytes /dev/envout) '/dev/envin)])
      (for ([line (in-lines /dev/envin 'any)])
        (define name=value (environment-variables-try-set! msvc-env line))

        (when (pair? name=value)
          (dtrace-trace #:topic 'env "~a = ~a"
                        (car name=value)
                        (cdr name=value))))
      
      (unless (not &env)
        (set-box! &env msvc-env))
      
      msvc-env)))

(define msvc-environment-variables : (-> Environment-Variables)
  (lambda []
    (or (unbox &msvc-env)
        (let-values ([(full-path path) (msvc-vcvarsall-path #true 'env)])
          (msvc-make-envs full-path path &msvc-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (c-register-compiler 'msvc '(flags macros includes infile outfile) 
                       #:macros msvc-cpp-macros #:includes msvc-include-paths #:flags msvc-compile-flags
                       #:outfile (msvc-make-outfile "Fo")
                       #:basename msvc-basename #:find-compiler msvc-path
                       #:env msvc-environment-variables)
  
  (c-register-linker 'msvc '(flags infiles libraries outfile "/link" libpath subsystem ldflags)
                     #:flags msvc-linker-flags #:subsystem msvc-subsystem-flags
                     #:libpaths msvc-linker-libpaths #:libraries msvc-linker-libraries
                     #:outfile (msvc-make-outfile "Fe")
                     #:basename msvc-basename #:find-linker msvc-path
                     #:env msvc-environment-variables))
  