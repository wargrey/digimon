#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require "../../../dtrace.rkt")
(require "../../../cc.rkt")

(require "../../../filesystem.rkt")
(require "../../../predicate.rkt")

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
   [subsystem : (Option Symbol)] ; `#false` means building `shared object`
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

    (map cc-launcher-filter maybe-launchers)))

(define digimon-native-files->launcher-names : (-> (Listof CC-Launcher-Name) (Listof Path) (Listof CC-Launcher-Name))
  (lambda [info-targets targets]
    (map cc-launcher-filter
         (for/list : (Listof Any) ([p (in-list targets)])
           (let ([info-p (assoc p info-targets)])
             (cond [(pair? info-p) info-p]
                   [else (cons p (list 'CONSOLE))]))))))

(define make-depcc-specs : (-> (Listof CC-Launcher-Name) (Listof String) Native-Subpath-Datum Boolean Wisemon-Specification)
  (let ([cpp-src-filter (λ [[file : Path]] : Boolean (regexp-match? #px"\\.c(pp)?$" file))])
    (lambda [launchers incdirs subnative debug?]
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
                ([dep.c (in-list all-depsrcs)] #:when (file-exists? dep.c))
        (define cpp-file? : Boolean (eq? (cc-lang-from-extension dep.c) 'cpp))
        (define deps.h : (Listof Path) (c-include-headers dep.c includes #:topic (current-make-phony-goal)))
        
        (list* (let ([dep++.o (assert (c-source->object-file dep.c 'cpp #:subnative subnative #:debug? debug?))])
                 (wisemon-spec dep++.o #:^ (cons dep.c deps.h)
                               #:- (c-compile #:cpp? #true #:verbose? (compiler-verbose) #:debug? debug?
                                              #:includes includes #:macros macros
                                              dep.c dep++.o)))
               
               (cond [(not cpp-file?)
                      (let ([dep.o (assert (c-source->object-file dep.c 'c #:subnative subnative #:debug? debug?))])
                        (cons (wisemon-spec dep.o #:^ (cons dep.c deps.h)
                                            #:- (c-compile #:cpp? #false #:verbose? (compiler-verbose) #:debug? debug?
                                                           #:includes includes #:macros macros
                                                           dep.c dep.o))
                              specs))]
                     [else specs]))))))

(define make-cc-specs : (-> (Listof CC-Launcher-Name) (Listof String) Native-Subpath-Datum Boolean Wisemon-Specification)
  (lambda [launchers incdirs subnative debug?]
    (define local-info.rkt : Path (digimon-path 'info))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([launcher (in-list launchers)])
      (define-values (native.c info) (values (car launcher) (cdr launcher)))
      (define lang : Symbol (or (cc-launcher-info-lang info) (cc-lang-from-extension native.c)))
      (define cpp? : Boolean (eq? lang 'cpp))
      
      (define native : Path
        (assert
         (if (cc-launcher-info-subsystem info)
             (c-source->executable-file native.c #false (cc-launcher-info-name info) #:subnative subnative #:debug? debug?)
             (c-source->shared-object-file native.c #false (cc-launcher-info-name info) #:subnative subnative #:debug? debug?))))

      (define native.o : Path (assert (c-source->object-file native.c lang #:subnative subnative #:debug? debug?)))

      (define includes : (Listof Path)
        (c-include-headers #:check-source? #true #:topic (current-make-phony-goal)
                           native.c (cc-launcher-info-includes info)))
      
      (define objects : (Listof Path)
        (let ([depobjs (c-headers->files includes (λ [[dep.c : Path]] (c-source->object-file dep.c lang #:subnative subnative #:debug? debug?)))])
          (remove-duplicates
           (cond [(member native.o depobjs) depobjs]
                 [else (cons native.o depobjs)]))))

      (define headers : (Listof Wisemon-Spec)
        (if (not (cc-launcher-info-subsystem info)) ; headers for shared object
            (let ([target-rootdir (assert (path-only native))]
                  [target-subdir (string-replace (path->string (path-replace-extension (assert (file-name-from-path native.c)) #"")) "." "_")]
                  [source-rootdir (assert (path-only native.c))])
              (for/fold ([ss : (Listof Wisemon-Spec) null])
                        ([header.h (in-list includes)])
                (define target-tail (find-relative-path source-rootdir header.h))
                (define spec : (Option Wisemon-Spec)
                  (and (relative-path? target-tail)
                       (let ([tails (explode-path target-tail)])
                         (and (pair? tails)
                              (andmap path? tails)
                              (let ([target (apply build-path target-rootdir target-subdir tails)])
                                (wisemon-spec target #:^ (list header.h)
                                              #:- (cc-header-sed target header.h)))))))

                (cond [(not spec) ss]
                      [else (cons spec ss)])))
            null))

      ; keep the order of building
      (append headers specs
              ; TODO: why includes duplicate inside the spec, but be okay outside the spec
              (list (wisemon-spec native.o #:^ (list* local-info.rkt native.c (c-include-headers #:check-source? #false #:topic (current-make-phony-goal)
                                                                                                 native.c (cc-launcher-info-includes info)))
                                  #:- (c-compile #:cpp? cpp? #:verbose? (compiler-verbose) #:debug? debug?
                                                 #:macros (cc-launcher-info-macros info)
                                                 #:includes (append incdirs (cc-launcher-info-includes info))
                                                 native.c native.o))
                    (wisemon-spec native #:^ (cons local-info.rkt (append objects (wisemon-targets-flatten headers)))
                                  #:- (c-link #:cpp? cpp? #:verbose? (compiler-verbose)
                                              #:subsystem (cc-launcher-info-subsystem info) #:entry (cc-launcher-info-entry info)
                                              #:libpaths (cc-launcher-info-libpaths info) #:libraries (cc-launcher-info-libraries info)
                                              #:postask (if (cc-launcher-info-subsystem info) void void)
                                              objects native)))))))

(define make-cc : (-> String (Option Info-Ref) Boolean Any)
  (lambda [digimon info-ref debug?]
    (define launchers : (Listof CC-Launcher-Name)
      (let ([info-targets (if (not info-ref) null (find-digimon-native-launcher-names info-ref))]
            [real-targets (current-make-real-targets)])
        (cond [(pair? real-targets) (digimon-native-files->launcher-names info-targets real-targets)]
              [else info-targets])))

    (when (pair? launchers)
      (define incdirs : (Listof String) (if (not info-ref) null (list (path->string (digimon-path 'zone)))))
      (define subnative : Native-Subpath-Datum (and info-ref (datum-filter (info-ref 'native-compiled-subpath (λ [] #false)) native-subpath-datum?)))
      (define depcc-specs : Wisemon-Specification (make-depcc-specs launchers incdirs subnative debug?))
      (define cc-specs : Wisemon-Specification (make-cc-specs launchers incdirs subnative debug?))
      
      (when (or info-ref)
        (wisemon-compile (current-directory) digimon info-ref))
      
      (wisemon-make (append depcc-specs cc-specs)
                    (wisemon-targets-flatten cc-specs)))))

(define make~release : Make-Free-Phony
  (lambda [digimon info-ref]
    (make-cc digimon info-ref #false)))

(define make~debug : Make-Free-Phony
  (lambda [digimon info-ref]
    (make-cc digimon info-ref #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-launcher-filter : (-> Any CC-Launcher-Name)
  (lambda [native]
    (if (and (pair? native)
             (or (string? (car native))
                 (path? (car native))))
        (let* ([p (car native)]
               [config (cdr native)]
               [native.cc (cond [(relative-path? p) (build-path (current-directory) (path-normalize/system p))]
                                [(string? p) (string->path p)]
                                [else p])])
          (cons native.cc
                (cond [(cc-launcher-info? config) config]
                      [else (cc-filter-name config)])))
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
                     [(so dll dylib) (partition lang biname (or subsystem #false) entry srehto rest)]
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

(define cc-header-sed : (-> Path Path Void)
  (lambda [target source]
    (make-parent-directory* target)

    (parameterize ([current-custodian (make-custodian)])
      (define /dev/stdin (open-input-file source))
      (define /dev/stdout (open-output-file target #:exists 'truncate/replace))

      (dtrace-info #:topic 'ld "~a ~a ~a" (object-name cc-header-sed) source target)

      (let sed ([protected-level : Natural 0])
        (define line (read-line /dev/stdin))
        (when (string? line)
          (define begin-exclude? : Boolean (regexp-match? #px"^\\s*//\\s*#[|]\\s*[Pp][Rr][Oo][Tt][Ee][Cc][Tt][Ee][Dd][-][Oo][Uu][Tt]\\s*$" line))

          (when (and (= protected-level 0) (not begin-exclude?))
            (fprintf /dev/stdout "~a~n" (string-replace line #px"__(ffi|lambda)__\\s+" "" #:all? #false))
            (flush-output /dev/stdout))

          (cond [(and begin-exclude?) (sed (add1 protected-level))]
                [(regexp-match? #px"^\\s*//\\s*[|]#\\s*$" line) (sed (if (> protected-level 0) (sub1 protected-level) 0))]
                [else (sed protected-level)])))

      (custodian-shutdown-all (current-custodian)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc #:phony make~release
                           #:desc "Build the collection as a C/C++ project [RELEASE]"))

(define cc-dbg-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc-dbg #:phony make~debug
                           #:desc "Build the collection as a C/C++ project [DEBUG]"))
