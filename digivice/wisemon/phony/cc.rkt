#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../../cc.rkt")

(require "../../../filesystem.rkt")
(require "../../../digitama/system.rkt")
(require "../../../digitama/toolchain/cc/configuration.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Launcher-Name (Pairof Path CC-Launcher-Info))

(struct cc-launcher-info
  ([lang : (Option Symbol)]
   [name : (Option String)]
   [subsystem : (Option Symbol)]
   [entry : (Option Keyword)]
   [macros : (Listof C-Compiler-Macro)]
   [includes : (Listof C-Toolchain-Path-String)]
   [libpaths : (Listof C-Toolchain-Path-String)]
   [libraries : (Listof C-Link-Library)])
  #:type-name CC-Launcher-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-native-launcher-names : (-> Info-Ref (Listof CC-Launcher-Name))
  (lambda [info-ref]
    (define maybe-launchers (info-ref 'native-launcher-names (λ [] null)))

    (unless (list? maybe-launchers)
      (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" maybe-launchers))

    (filter-map cc-launcher-filter maybe-launchers)))

(define digimon-native-files->launcher-names : (-> (Listof CC-Launcher-Name) (Listof Path) (Listof CC-Launcher-Name))
  (lambda [info-targets targets]
    (filter-map cc-launcher-filter
                (for/list : (Listof Any) ([p (in-list targets)])
                  (let ([info-p (assoc p info-targets)])
                    (cond [(pair? info-p) info-p]
                          [else (cons p (list 'CONSOLE))]))))))

(define make-depcc-specs : (-> (Listof CC-Launcher-Name) (Listof String) Wisemon-Specification)
  (let ([cpp-src-filter (λ [[file : Path]] : Boolean (regexp-match? #px"\\.c(pp)?$" file))])
    (lambda [launchers incdirs]
      (define-values (macros includes)
        (let-values ([(ms is) (for/fold ([macros : (Listof C-Compiler-Macro) null]
                                         [includes : (Listof C-Toolchain-Path-String) incdirs])
                                        ([launcher (in-list launchers)])
                                (let ([info (cdr launcher)])
                                  (values (append macros (cc-launcher-info-macros info))
                                          (append includes (cc-launcher-info-includes info)))))])
          (values (remove-duplicates ms) (c-path-flatten is))))

      (define dep-rootdirs : (Listof Path)
        (for/fold ([dirs : (Listof Path) (list (current-directory))])
                  ([extra (in-list includes)] #:when (relative-path? extra))
          (define rootdir (simplify-path (build-path (current-directory) extra)))
          (cond [(member rootdir dirs) dirs]
                [else (cons rootdir dirs)])))

      (define all-depsrcs : (Listof Path)
        (remove-duplicates
         (apply append
                (for/list : (Listof (Listof Path)) ([dir (in-list dep-rootdirs)] #:when (directory-exists? dir))
                  (find-digimon-files cpp-src-filter dir)))))
      
      (for/fold ([specs : Wisemon-Specification null])
                ([dep.c (in-list all-depsrcs)])
        (define cpp-file? : Boolean (eq? (cc-lang-from-extension dep.c) 'cpp))
        (define deps.h : (Listof Path) (c-include-headers dep.c includes #:topic (current-make-phony-goal)))
        (define dep++.o : Path (assert (c-source->object-file dep.c 'cpp)))
        
        (list* (wisemon-spec dep++.o #:^ (cons dep.c deps.h)
                             #:- (c-compile #:cpp? #true #:verbose? (compiler-verbose)
                                            #:includes includes #:macros macros
                                            dep.c dep++.o))
               
               (cond [(not cpp-file?)
                      (let ([dep.o (assert (c-source->object-file dep.c 'c))])
                        (cons (wisemon-spec dep.o #:^ (cons dep.c deps.h)
                                            #:- (c-compile #:cpp? #false #:verbose? (compiler-verbose)
                                                           #:includes includes #:macros macros
                                                           dep.c dep.o))
                              specs))]
                     [else specs]))))))

(define make-cc-specs : (-> (Listof CC-Launcher-Name) (Listof String) Wisemon-Specification)
  (lambda [launchers incdirs]
    (for/fold ([specs : Wisemon-Specification null])
              ([launcher (in-list launchers)])
      (define-values (native.c info) (values (car launcher) (cdr launcher)))
      (define lang : Symbol (or (cc-launcher-info-lang info) (cc-lang-from-extension native.c)))
      (define cpp? : Boolean (eq? lang 'cpp))
      (define native : Path (assert (c-source->executable-file native.c #false (cc-launcher-info-name info))))

      (define native.o : Path (assert (c-source->object-file native.c lang)))
      (define objects : (Listof Path)
        (let ([depobjs (c-headers->files (c-include-headers native.c (cc-launcher-info-includes info) #:check-source? #true #:topic (current-make-phony-goal))
                                         (λ [[dep.c : Path]] (c-source->object-file dep.c lang)))])
          (remove-duplicates
           (cond [(member native.o depobjs) depobjs]
                 [else (cons native.o depobjs)]))))

      ; TODO: why includes duplicate inside the spec, but be okay outside the spec
      (list* (wisemon-spec native.o #:^ (cons native.c (c-include-headers native.c (cc-launcher-info-includes info) #:topic (current-make-phony-goal)))
                           #:- (c-compile #:cpp? cpp? #:verbose? (compiler-verbose) #:macros (cc-launcher-info-macros info)
                                          #:includes (append incdirs (cc-launcher-info-includes info))
                                          native.c native.o))
             (wisemon-spec native #:^ objects
                           #:- (c-link #:cpp? cpp? #:verbose? (compiler-verbose)
                                       #:subsystem (cc-launcher-info-subsystem info) #:entry (cc-launcher-info-entry info)
                                       #:libpaths (cc-launcher-info-libpaths info) #:libraries (cc-launcher-info-libraries info)
                                       objects native))
             specs))))
    
(define make~cc : Make-Free-Phony
  (lambda [digimon info-ref]
    (define launchers : (Listof CC-Launcher-Name)
      (let ([info-targets (if (not info-ref) null (find-digimon-native-launcher-names info-ref))]
            [real-targets (current-make-real-targets)])
        (cond [(pair? real-targets) (digimon-native-files->launcher-names info-targets real-targets)]
              [else info-targets])))

    (when (pair? launchers)
      (define incdirs : (Listof String) (if (not info-ref) null (list (path->string (digimon-path 'zone)))))
      (define depcc-specs : Wisemon-Specification (make-depcc-specs launchers incdirs))
      (define cc-specs : Wisemon-Specification (make-cc-specs launchers incdirs))
      
      (when (or info-ref)
        (wisemon-compile (current-directory) digimon info-ref))
      
      (wisemon-make (append depcc-specs cc-specs)
                    (wisemon-targets-flatten cc-specs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-launcher-filter : (-> Any (Option CC-Launcher-Name))
  (lambda [native]
    (if (and (pair? native) (or (string? (car native)) (path? (car native))))
        (let* ([p (car native)]
               [config (cdr native)]
               [native.cc (cond [(relative-path? p) (build-path (current-directory) (path-normalize/system p))]
                                [(string? p) (string->path p)]
                                [else p])])
          (and (file-exists? native.cc)
               (cons native.cc
                     (cond [(cc-launcher-info? config) config]
                           [else (cc-filter-name config)]))))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-filter-name : (-> Any CC-Launcher-Info)
  (lambda [argv]
    (let partition ([lang : (Option Symbol) #false]
                    [biname : (Option String) #false]
                    [subsystem : (Option Symbol) #false]
                    [entry : (Option Keyword) #false]
                    [srehto : (Listof Any) null]
                    [options : (Listof Any) (if (list? argv) argv (list argv))])
      (if (pair? options)
          (let-values ([(self rest) (values (car options) (cdr options))])
            (cond [(pair? self) (partition lang biname subsystem entry (cons self srehto) rest)]
                  [(symbol? self)
                   (case self
                     [(C c) (partition (or lang 'c) biname subsystem entry srehto rest)]
                     [(C++ c++ Cpp cpp) (partition (or lang 'cpp) biname subsystem entry srehto rest)]
                     [(console) (partition lang biname (or subsystem 'CONSOLE) entry srehto rest)]
                     [(windows desktop) (partition lang biname (or subsystem 'WINDOWS) entry srehto rest)]
                     [else (partition lang biname (or subsystem self) entry srehto rest)])]
                  [(keyword? self) (partition lang biname subsystem (or entry self) srehto rest)]
                  [(string? self) (partition lang (or biname self) subsystem entry srehto rest)]
                  [else (partition lang biname subsystem entry srehto rest)]))
          (let-values ([(macros includes libpaths libraries) (c-configuration-filter (reverse srehto) digimon-system)])
            (cc-launcher-info lang biname subsystem entry macros includes libpaths libraries))))))

(define cc-lang-from-extension : (->* (Path) (Bytes) Symbol)
  (lambda [native.cc [fallback-ext #".cpp"]]
    (string->symbol
     (string-downcase
      (bytes->string/utf-8
       (subbytes (or (path-get-extension native.cc) fallback-ext) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc #:phony make~cc
                           #:desc "Build the collection as a C/C++ project preprocessed by Racket"))
