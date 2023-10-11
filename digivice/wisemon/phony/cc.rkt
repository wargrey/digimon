#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)
(require racket/bool)

(require "../../../dtrace.rkt")
(require "../../../cc.rkt")

(require "../../../filesystem.rkt")
(require "../../../predicate.rkt")
(require "../../../function.rkt")

(require "../../../digitama/path.rkt")
(require "../../../digitama/system.rkt")
(require "../../../digitama/toolchain/cc/configuration.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
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

(struct cc-native-tree
  ([root : Native-Subpath]
   [bindir : Native-Subpath]
   [incdir : Native-Subpath]
   [libdir : Native-Subpath])
  #:type-name CC-Native-Tree
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-native-launcher-names : (->* (Info-Ref Boolean) ((Option Symbol)) (Listof CC-Launcher-Name))
  (lambda [info-ref debug? [force-lang #false]]
    (define maybe-launchers (info-ref 'native-launcher-names (λ [] null)))

    (unless (list? maybe-launchers)
      (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" maybe-launchers))

    (for/list ([launcher (in-list maybe-launchers)])
      (cc-launcher-filter launcher debug? force-lang))))

(define digimon-native-files->launcher-names : (->* ((Listof CC-Launcher-Name) (Listof Path) Boolean)
                                                    ((Option Symbol))
                                                    (Listof CC-Launcher-Name))
  (lambda [info-targets targets debug? [force-lang #false]]
    (define launchers
      (for/list : (Listof Any) ([p (in-list targets)])
        (let ([info-p (assoc p info-targets)])
          (cond [(pair? info-p) info-p]
                [else (cons p (list 'CONSOLE))]))))

    (for/list ([launcher (in-list launchers)])
      (cc-launcher-filter launcher debug? force-lang))))

(define make-object-spec : (->* (Path Path Boolean (Listof C-Compiler-Macro) (Listof C-Toolchain-Path-String) Boolean)
                                ((Listof String))
                                Wisemon-Spec)
  (lambda [source.c object.o cpp-file? macros includes debug? [extra-includes null]]
    (wisemon-spec object.o #:^ (cons source.c (c-include-headers source.c includes #:check-source? #false #:topic (current-make-phony-goal)))
                  #:- (c-compile #:cpp? cpp-file? #:verbose? (compiler-verbose) #:debug? debug?
                                 #:includes (append extra-includes includes) #:macros macros
                                 source.c object.o))))

(define make-cc-specs : (-> (Listof CC-Launcher-Name) (Listof String) CC-Native-Tree Boolean Wisemon-Specification)
  (lambda [launchers extra-incdirs ntree debug?]
    (for/fold ([specs : Wisemon-Specification null])
              ([launcher (in-list launchers)])
      (define-values (native.c info) (values (car launcher) (cdr launcher)))
      (define self:bin? : Boolean (and (cc-launcher-info-subsystem info) #true))
      (define lang : Symbol (or (cc-launcher-info-lang info) (cc-lang-from-extension native.c)))
      (define macros : (Listof C-Compiler-Macro) (cc-launcher-info-macros info))
      (define incdirs : (Listof C-Toolchain-Path-String) (cc-launcher-info-includes info))
      (define libs : (Listof C-Link-Library) (cc-launcher-info-libraries info))
      (define libname? : Boolean (not (eq? digimon-system 'windows)))
      (define cpp? : Boolean (eq? lang 'cpp))
      
      (define includes : (Listof Path)
        (c-include-headers #:check-source? #true #:topic (current-make-phony-goal)
                           native.c (cc-launcher-info-includes info)))

      (define sources : (Listof Path) (c-headers->sources includes))
      (define-values (dep-objects object-specs)
        (for/fold ([objs : (Listof Path) null]
                   [specs : (Listof Wisemon-Spec) null])
                  ([src (in-list sources)])
          (define obj.o (assert (c-source->object-file src lang #:subnative (cc-native-tree-root ntree))))
          (values (cons obj.o objs)
                  (cons (make-object-spec src obj.o cpp? macros incdirs debug? extra-incdirs)
                        specs))))

      (define native : Path
        (assert
         (if (or self:bin?)
             (c-source->executable-file
              #:subnative (append (cc-native-tree-root ntree) (cc-native-tree-bindir ntree))
              native.c #false (cc-launcher-info-name info))
             (c-source->shared-object-file
              #:subnative (append (cc-native-tree-root ntree) (cc-native-tree-libdir ntree)) #:lib-prefixed? libname?
              native.c #false (cc-launcher-info-name info)))))

      (define native.o : Path (assert (c-source->object-file native.c lang #:subnative (cc-native-tree-root ntree))))
      (define deplibs : (Listof Path) (cc-dependent-shared-objects libs specs libname?))
      (define objects : (Listof Path) (let ([dos (remove-duplicates dep-objects)]) (if (member native.o dos) dos (cons native.o dos))))

      (define header-specs : (Listof Wisemon-Spec)
        (if (not (cc-launcher-info-subsystem info)) ; headers for shared object
            (let ([target-rootdir (assert (path-only native.o))]
                  [target-incdir (native-subpath->path (cc-native-tree-incdir ntree))]
                  [target-namedir (string-replace (path->string (path-replace-extension (assert (file-name-from-path native.c)) #"")) "." "_")]
                  [source-rootdir (assert (path-only native.c))])
              (for/fold ([ss : (Listof Wisemon-Spec) null])
                        ([header.h (in-list includes)])
                (define target-tail (find-relative-path source-rootdir header.h))
                (define spec : (Option Wisemon-Spec)
                  (and (relative-path? target-tail)
                       (let ([tails (explode-path target-tail)])
                         (and (pair? tails)
                              (andmap path? tails)
                              (let ([target (if (not target-incdir)
                                                (apply build-path target-rootdir target-namedir tails)
                                                (apply build-path target-rootdir target-incdir target-namedir tails))])
                                (wisemon-spec target #:^ (list header.h)
                                              #:- (cc-header-sed target header.h)))))))

                (cond [(not spec) ss]
                      [else (cons spec ss)])))
            null))

      (define self-specs : (Listof Wisemon-Spec)
        ; TODO: why includes duplicate inside the spec, but be okay outside the spec
        (list (wisemon-spec native #:^ (append deplibs objects (wisemon-targets-flatten header-specs))
                            #:- (c-link #:cpp? cpp? #:verbose? (compiler-verbose)
                                        #:subsystem (cc-launcher-info-subsystem info) #:entry (cc-launcher-info-entry info)
                                        #:libpaths (cc-launcher-info-libpaths info) #:libraries libs
                                        objects native))
              (make-object-spec native.c native.o cpp? macros incdirs debug? extra-incdirs)))

      ; WARNING: Order matters
      ; force the `make` to check binaries before shared libraries
      ;   so that updated shared libraries will trigger the remaking of binaries
      (if (or self:bin?)
          (append self-specs specs header-specs object-specs)
          (append specs self-specs header-specs object-specs)))))

(define make-cc-spec+targets : (-> (Option Info-Ref) Boolean (Option Symbol)
                                   (Values (Option (Pairof Wisemon-Specification (Listof CC-Launcher-Name)))
                                           (Listof Path)))
  (lambda [info-ref debug? force-lang]
    (define launchers : (Listof CC-Launcher-Name)
      (let ([info-targets (if (not info-ref) null (find-digimon-native-launcher-names info-ref debug? force-lang))]
            [real-targets (current-make-real-targets)])
        (cond [(pair? real-targets) (digimon-native-files->launcher-names info-targets real-targets debug? force-lang)]
              [else info-targets])))
    
    (if (pair? launchers)
        (let* ([incdirs (if (not info-ref) null (list (path->string (digimon-path 'zone))))]
               [cc-specs (make-cc-specs launchers incdirs (cc-subpath-filter info-ref debug?) debug?)])
          (values (cons cc-specs launchers) (wisemon-targets-flatten cc-specs)))
        (values #false null))))

(define make-cc : (-> (Option Info-Ref) Boolean Any)
  (lambda [info-ref debug?]
    (define-values (cc-specs.launchers targets) (make-cc-spec+targets info-ref debug? #false))

    (when (or cc-specs.launchers)
      (wisemon-make (car cc-specs.launchers) targets))))

(define make~release : Make-Free-Phony
  (lambda [digimon info-ref]
    (make-cc info-ref #false)))

(define make~debug : Make-Free-Phony
  (lambda [digimon info-ref]
    (make-cc info-ref #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-launcher-filter : (-> Any Boolean (Option Symbol) CC-Launcher-Name)
  (lambda [native debug? force-lang]
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
                      [else (cc-filter-name config debug? force-lang)])))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-subpath-filter : (-> (Option Info-Ref) Boolean CC-Native-Tree)
  (let ([default-subroot : Native-Subpath (list 'subpath)]
        [default-bindir : Native-Subpath (list "bin")]
        [default-incdir : Native-Subpath (list "include")]
        [default-libdir : Native-Subpath (list "lib")]
        [default-release : Native-Subpath (list "release")]
        [default-debug : Native-Subpath (list "debug")])
    (lambda [info-ref debug?]
      (define-values (native-compiled-dist default-dist)
        (if debug?
            (values 'native-compiled-debug default-debug)
            (values 'native-compiled-release default-release)))
      
      (if (or info-ref)
          (let* ([subroot (or (datum-filter (info-ref 'native-compiled-subpath λfalse) native-subpath?) default-subroot)]
                 [dstdir (or (datum-filter (info-ref native-compiled-dist λfalse) native-subpath?) default-dist)]
                 [bindir (or (datum-filter (info-ref 'native-compiled-bindir λfalse) native-subpath?) default-bindir)]
                 [incdir (or (datum-filter (info-ref 'native-compiled-incdir λfalse) native-subpath?) default-incdir)]
                 [libdir (or (datum-filter (info-ref 'native-compiled-libdir λfalse) native-subpath?) default-libdir)])
            (cc-native-tree (append subroot dstdir) bindir incdir libdir))
          (cc-native-tree (append default-subroot default-dist) default-bindir default-incdir default-libdir)))))

(define cc-libname-filter : (-> (Listof C-Link-Library) (Listof Symbol))
  (lambda [libs]
    (for/fold ([libaries : (Listof Symbol) null])
              ([lib (in-list libs)])
      (cond [(symbol? lib) (cons lib libaries)]
            [else (append (cdr lib) libaries)]))))

(define cc-filter-name : (-> Any Boolean (Option Symbol) CC-Launcher-Info)
  (lambda [argv debug? force-lang]
    (let partition ([lang : (Option Symbol) force-lang]
                    [biname : (Option String) #false]
                    [subsystem : (Option Symbol) #false]
                    [entry : (Option Keyword) #false]
                    [srehto : (Listof Any) null]
                    [options : (Listof Any) (if (list? argv) argv (list argv))])
      (if (pair? options)
          (let-values ([(self rest) (values (car options) (cdr options))])
            (cond [(list? self)
                   (let-values ([(maybe-distr subopts) (values (car self) (cdr self))])
                     (cond [(not (keyword? maybe-distr)) (partition lang biname subsystem entry (cons self srehto) rest)]
                           [(xor debug? (memq maybe-distr '(#:DEBUG #:debug))) (partition lang biname subsystem entry srehto rest)]
                           [(list? subopts) (partition lang biname subsystem entry srehto (append subopts rest))]
                           [else (partition lang biname subsystem entry srehto (cons subopts rest))]))]
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
        (define line (read-line /dev/stdin 'any))
        (when (string? line)
          (define begin-exclude? : Boolean (regexp-match? #px"^\\s*//\\s*#[|]\\s*[Pp][Rr][Oo][Tt][Ee][Cc][Tt][Ee][Dd][-][Oo][Uu][Tt]\\s*$" line))

          (when (and (= protected-level 0) (not begin-exclude?))
            (fprintf /dev/stdout "~a~n" (string-replace line #px"__(ffi|lambda)__\\s+" "" #:all? #false))
            (flush-output /dev/stdout))

          (cond [(and begin-exclude?) (sed (add1 protected-level))]
                [(regexp-match? #px"^\\s*//\\s*[|]#\\s*$" line) (sed (if (> protected-level 0) (sub1 protected-level) 0))]
                [else (sed protected-level)])))

      (custodian-shutdown-all (current-custodian)))))

(define cc-dependent-shared-objects : (-> (Listof C-Link-Library) Wisemon-Specification Boolean (Listof Path))
  (lambda [libs specs libname?]
    (define targets : (Listof Path) (wisemon-targets-flatten specs))
    
    (if (pair? targets)
        (let ([libs (cc-libname-filter libs)])
          (for/fold ([deplibs : (Listof Path) null])
                    ([target (in-list targets)])
            (define libname.so (file-name-from-path target))
            (if (or libname.so)
                (let ([name (string->symbol (native-shared-object-name-restore libname.so libname?))])
                  (if (memq name libs) (cons target deplibs) deplibs))
                deplibs)))
        null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc #:phony make~release
                           #:desc "Build the collection as a C/C++ project [RELEASE]"))

(define cc-dbg-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc-dbg #:phony make~debug
                           #:desc "Build the collection as a C/C++ project [DEBUG]"))
