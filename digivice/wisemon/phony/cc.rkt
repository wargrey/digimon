#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)

(require racket/list)
(require racket/bool)

(require "../../../dtrace.rkt")

(require "../../../filesystem.rkt")
(require "../../../predicate.rkt")

(require "../../../digitama/exec.rkt")
(require "../../../digitama/path.rkt")
(require "../../../digitama/toolchain/cc/cc.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../native.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Launcher-Name (Pairof Path CC-Launcher-Info))

(struct cc-launcher-info
  ([lang : (Option Symbol)]
   [name : (Option String)]
   [subsystem : (Option Symbol)] ; `#false` means building `shared object`
   [entry : (Option Keyword)]
   [compilers : (Listof Symbol)]
   [macros : (Listof C-Compiler-Macro)]
   [includes : (Listof C-Toolchain-Path-String)]
   [libpaths : (Listof C-Toolchain-Path-String)]
   [libraries : (Listof C-Link-Library)])
  #:type-name CC-Launcher-Info
  #:transparent)

(struct cc-native-tree
  ([root : Native-Subpath]
   [bindir : Native-Subpath]
   [libdir : Native-Subpath])
  #:type-name CC-Native-Tree
  #:transparent)

(struct cc-destination-tree
  ([drive : Path-String]
   [root : Path-String]
   [incdir : Path-String]
   [libdir : Path-String])
  #:type-name CC-Destination-Tree
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-native-launcher-names : (->* (Info-Ref Boolean) ((Option Symbol)) (Listof CC-Launcher-Name))
  (lambda [info-ref debug? [force-lang #false]]
    (define-values (launchers default-compilers) (find-digimon-native-launcher-names* info-ref debug? force-lang))
    launchers))
  
  
(define find-digimon-native-launcher-names* : (->* (Info-Ref Boolean) ((Option Symbol)) (Values (Listof CC-Launcher-Name) (Listof Symbol)))
  (lambda [info-ref debug? [force-lang #false]]
    (define maybe-launchers (info-ref 'native-launcher-names (λ [] null)))
    (define default-compilers (cc-launcher-compiler-filter info-ref))

    (unless (list? maybe-launchers)
      (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" maybe-launchers))

    (values (for/list ([launcher (in-list maybe-launchers)])
              (cc-launcher-filter launcher debug? force-lang default-compilers))
            default-compilers)))

(define digimon-native-files->launcher-names : (->* ((Listof CC-Launcher-Name) (Listof Path) Boolean (Listof Symbol))
                                                    ((Option Symbol))
                                                    (Values (Listof CC-Launcher-Name) (Listof CC-Launcher-Name)))
  (lambda [info-targets targets debug? default-compilers [force-lang #false]]
    (define-values (known-launchers unknown-paths)
      (for/fold ([known-launchers : (Listof CC-Launcher-Name) null]
                 [unknown-paths : (Listof Path) null])
                ([p (in-list targets)])
        (let ([info-p (assoc p info-targets)])
          (if (pair? info-p)
              (values (cons info-p known-launchers) unknown-paths)
              (values known-launchers (cons p unknown-paths))))))

    (values (for/list ([launcher (in-list (reverse known-launchers))])
              (cc-launcher-filter launcher debug? force-lang default-compilers))
            (for/list ([path (in-list (reverse unknown-paths))])
              (cc-launcher-filter (cons path (list 'CONSOLE))
                                  debug? force-lang default-compilers)))))

(define make-object-spec : (->* (Path Path
                                      Boolean (Listof Symbol) (Option CC-Standard-Version)
                                      (Listof C-Compiler-Macro) (Listof C-Toolchain-Path-String) Boolean)
                                ((Listof String) (Listof Path) (Listof Path))
                                Wisemon-Spec)
  (lambda [source.c object.o cpp-file? compilers standard macros includes debug? [extra-includes null] [extra-headers null] [extra-shared-objects null]]
    (wisemon-spec object.o #:^ (append (cons source.c (append extra-headers extra-shared-objects))
                                       (c-include-headers source.c includes #:check-source? #false #:topic (current-make-phony-goal)))
                  #:- (c-compile #:compilers compilers #:standard standard #:cpp? cpp-file?
                                 #:includes (append extra-includes includes) #:macros macros
                                 #:verbose? (compiler-verbose) #:debug? debug?
                                 source.c object.o))))

(define make-distributed-shared-object-spec : (-> Path Path Wisemon-Spec)
  (lambda [dest.so src.so]
    (define src.lib (path-replace-extension src.so library.ext))
    (define dest.lib (path-replace-extension dest.so library.ext))

    (wisemon-spec dest.so #:^ (list src.so)
                  #:- (fg-cp 'misc src.so dest.so)

                  ; MSVC also requires a partner static library
                  (when (file-exists? src.lib)
                    (fg-cp 'misc src.lib dest.lib)))))

(define make-header-specs : (-> (Listof (Pairof Path Path)) (Listof Wisemon-Spec))
  (lambda [headers]
    (let make-header-spec ([hs : (Listof (Pairof Path Path)) headers]
                           [ss : (Listof Wisemon-Spec) null])
      (if (pair? hs)
          (let ([dst.h (caar hs)]
                [src.h (cdar hs)])
            (define spec : Wisemon-Spec
              (wisemon-spec dst.h #:^ (list src.h)
                            #:- (cc-header-sed dst.h src.h)))
            (make-header-spec (cdr hs) (cons spec ss)))
          ss))))

(define make-cc-specs : (-> (Listof CC-Launcher-Name) (Listof String) CC-Native-Tree (Option CC-Destination-Tree) Boolean Wisemon-Specification)
  (lambda [launchers extra-incdirs build-tree dest-tree debug?]
    (for/fold ([specs : Wisemon-Specification null])
              ([launcher (in-list launchers)])
      (define-values (native.c info) (values (car launcher) (cdr launcher)))
      (define lang : Symbol (or (cc-launcher-info-lang info) (cc-lang-from-extension native.c)))
      (define cpp? : Boolean (eq? lang 'cpp))
      
      (define compilers : (Listof Symbol) (cc-launcher-info-compilers info))
      (define standard (c-language-standard-filter #%info (if cpp? 'native-cpp-standard 'native-c-standard)))

      (define self:bin? : Boolean (and (cc-launcher-info-subsystem info) #true))
      (define libname? : Boolean (not (eq? digimon-system 'windows)))

      (define X: : (Option Path-String) (and dest-tree (cc-destination-tree-drive dest-tree)))
      (define bin:build : Native-Subpath (append (cc-native-tree-root build-tree) (cc-native-tree-bindir build-tree)))
      (define lib:build : Native-Subpath (append (cc-native-tree-root build-tree) (cc-native-tree-libdir build-tree)))
      (define inc:target : (Listof Path) (if dest-tree (list (path-normalize/system (cc-destination-tree-incdir dest-tree) #:drive X:)) null))
      (define lib:target : (Listof Path) (if dest-tree (list (path-normalize/system (cc-destination-tree-libdir dest-tree) #:drive X:)) null))
      (define ->extpath : (Option (-> Path (Option (Pairof Path Path)))) (and (pair? lib:target) (cc-make-path->dest-path (car lib:target) lib:build)))
      
      (define macros : (Listof C-Compiler-Macro) (cc-launcher-info-macros info))
      (define incdirs : (Listof C-Toolchain-Path-String) (append inc:target (cc-launcher-info-includes info)))
      (define libdirs : (Listof C-Toolchain-Path-String) (append lib:target (cc-launcher-info-libpaths info)))
      (define libs : (Listof C-Link-Library) (cc-launcher-info-libraries info))
      (define deplibs : (Listof Path) (cc-dependent-shared-objects libs specs libname?))
      
      (define includes : (Listof Path)
        (c-include-headers #:check-source? #true #:topic (current-make-phony-goal)
                           native.c (cc-launcher-info-includes info)))

      (define native : Path
        (assert
         (if (or self:bin?)
             (c-source->executable-file #:subnative bin:build native.c #false (cc-launcher-info-name info))
             (c-source->shared-object-file #:subnative lib:build #:lib-prefixed? libname?
                                           native.c #false (cc-launcher-info-name info)))))

      (define dest-libs : (Option (Listof (Pairof Path Path))) (and ->extpath (filter-map ->extpath deplibs)))
      (define dest-self : (Option (Pairof Path Path)) (and ->extpath (not self:bin?) (->extpath native)))
      (define dest-incdir : (Option Path) (and (pair? inc:target) (build-path (car inc:target) (cc-library-name native.c))))
      
      (define dest-headers : (Option (Listof (Pairof Path Path)))
        (and dest-incdir (not (cc-launcher-info-subsystem info)) ; only send headers of shared objects to destination
             (cc-destination-headers (assert (path-only native.c)) includes dest-incdir)))
      
      ; (define extra-headers : (Listof Path) (if (pair? dest-headers) (map (inst car Path Path) dest-headers) null))
      (define extra-shared-objects : (Listof Path) (if (pair? dest-libs) (map (inst car Path Path) dest-libs) deplibs))
      
      (define header-specs : (Listof Wisemon-Spec) (make-header-specs (or dest-headers null)))
      (define extlib-specs : (Listof Wisemon-Spec) (if (not dest-self) null (list (make-distributed-shared-object-spec (car dest-self) (cdr dest-self)))))

      (define sources : (Listof Path) (c-headers->sources includes))
      (define-values (dep-objects object-specs)
        (for/fold ([objs : (Listof Path) null]
                   [specs : (Listof Wisemon-Spec) null])
                  ([src (in-list sources)])
          (define obj.o (assert (c-source->object-file src lang #:subnative (cc-native-tree-root build-tree))))
          (values (cons obj.o objs)
                  (cons (make-object-spec src obj.o cpp? compilers standard macros incdirs debug? extra-incdirs null extra-shared-objects)
                        specs))))

      (define native.o : Path (assert (c-source->object-file native.c lang #:subnative (cc-native-tree-root build-tree))))
      (define objects : (Listof Path)
        (let ([dos (remove-duplicates dep-objects)])
          (if (member native.o dos) dos (cons native.o dos))))

      (define self-specs : (Listof Wisemon-Spec)
        ; TODO: why includes duplicate inside the spec, but be okay outside the spec
        (list (wisemon-spec native #:^ (append ; don't burden objects with extra headers
                                        (if (pair? dest-headers) (map (inst car Path Path) dest-headers) null)
                                        objects)
                            #:- (c-link #:linkers compilers #:cpp? cpp?
                                        #:subsystem (cc-launcher-info-subsystem info) #:entry (cc-launcher-info-entry info)
                                        #:libpaths libdirs #:libraries libs
                                        #:verbose? (compiler-verbose)
                                        objects native))
              (make-object-spec native.c native.o cpp? compilers standard macros incdirs debug? extra-incdirs null extra-shared-objects)))

      (when (and dest-incdir dest-headers)
        (cc-clear-destination-include dest-incdir dest-headers))

      (append specs self-specs extlib-specs header-specs object-specs))))

(define make-cc-spec+targets : (-> (Option Info-Ref) Boolean (Option Symbol)
                                   (Values (Option (Pairof Wisemon-Specification (Listof CC-Launcher-Name)))
                                           (Listof Path)))
  (lambda [info-ref debug? force-lang]
    (define real-targets : (Listof Path) (current-make-real-targets))

    (define-values (info-launchers default-compilers)
      (cond [(not info-ref) (values null null)]
            [else (find-digimon-native-launcher-names* info-ref debug? force-lang)]))
    
    (define-values (real-launchers extra-real-launchers)
      (if (pair? real-targets)
          (digimon-native-files->launcher-names info-launchers real-targets debug? default-compilers force-lang)
          (values null null)))
    
    (if (or (pair? info-launchers) (pair? real-targets))
        (let* ([incdirs (if (not info-ref) null (list (path->string (digimon-path 'zone))))]
               [subfilter (cc-subpath-filter info-ref debug?)]
               [dstfilter (cc-destination-filter info-ref debug?)])
          (if (null? real-launchers)
              (let* ([actual-launchers (if (null? real-targets) info-launchers extra-real-launchers)]
                     [actual-specs (make-cc-specs actual-launchers incdirs subfilter dstfilter debug?)])
                (values (cons actual-specs actual-launchers)
                        (wisemon-targets-flatten actual-specs)))
              (let* ([info-specs (make-cc-specs info-launchers incdirs subfilter dstfilter debug?)]
                     [real-specs (make-cc-specs real-launchers incdirs subfilter dstfilter debug?)]
                     [extra-specs (make-cc-specs extra-real-launchers incdirs subfilter dstfilter debug?)])
                (values (cons (append info-specs extra-specs)
                              (append real-launchers extra-real-launchers))
                        (wisemon-targets-flatten (append real-specs extra-specs))))))
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
(define cc-launcher-compiler-filter : (-> Info-Ref (Listof Symbol))
  (lambda [info-ref]
    (c-compiler-filter info-ref "~a-toolchain-names" 'native)))

(define cc-launcher-filter : (-> Any Boolean (Option Symbol) (Listof Symbol) CC-Launcher-Name)
  (lambda [native debug? force-lang default-compilers]
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
                      [else (cc-filter-name config debug? default-compilers force-lang)])))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-subpath-filter : (-> (Option Info-Ref) Boolean CC-Native-Tree)
  (let ([default-subroot : Native-Subpath (list 'subpath)]
        [default-bindir : Native-Subpath (list "bin")]
        [default-libdir : Native-Subpath (list "lib")]
        [default-release : Native-Subpath (list "release")]
        [default-debug : Native-Subpath (list "debug")])
    (lambda [info-ref debug?]
      (define-values (native-compiled-dist default-dist)
        (if debug?
            (values 'native-compiled-debug default-debug)
            (values 'native-compiled-release default-release)))
      
      (if (or info-ref)
          (let ([subroot (or (datum-filter (info-ref 'native-compiled-subpath λfalse) native-subpath?) default-subroot)]
                [dstdir (or (datum-filter (info-ref native-compiled-dist λfalse) native-subpath?) default-dist)]
                [bindir (or (datum-filter (info-ref 'native-compiled-bindir λfalse) native-subpath?) default-bindir)]
                [libdir (or (datum-filter (info-ref 'native-compiled-libdir λfalse) native-subpath?) default-libdir)])
            (cc-native-tree (append subroot dstdir) bindir libdir))
          (cc-native-tree (append default-subroot default-dist) default-bindir default-libdir)))))

(define cc-destination-filter : (-> (Option Info-Ref) Boolean (Option CC-Destination-Tree))
  (let ([default-drive : String "C:"]
        [default-incdir : Native-Subpath (list "include")]
        [default-libdir : Native-Subpath (list "lib")])
    (lambda [info-ref debug?]
      (define-values (native-destination-subdist default-dist)
        (if debug?
            (values 'native-destination-debug null)
            (values 'native-destination-release null)))

      (and (or info-ref)
           (let ([root (datum-filter-map (info-ref 'native-destination-subroot λfalse) native-subpath? native-subpath->path)])
             (and root
                  (let ([drive (or (datum-filter (info-ref 'native-destination-drive λfalse) string? path?) default-drive)]
                        [dstdir (datum-map (info-ref native-destination-subdist λfalse) native-subpath? default-dist native-subpath->path)]
                        [incdir (datum-map (info-ref 'native-destination-incdir λfalse) native-subpath? default-incdir native-subpath->path)]
                        [libdir (datum-map (info-ref 'native-destination-libdir λfalse) native-subpath? default-libdir native-subpath->path)])
                    (cc-destination-tree drive (build-path "/" root)
                                         (build-path* "/" root incdir)
                                         (build-path* "/" root libdir dstdir)))))))))

(define cc-libname-filter : (-> (Listof C-Link-Library) (Listof Symbol))
  (lambda [libs]
    (for/fold ([libaries : (Listof Symbol) null])
              ([lib (in-list libs)])
      (cond [(symbol? lib) (cons lib libaries)]
            [else (append (cdr lib) libaries)]))))

(define cc-filter-name : (-> Any Boolean (Listof Symbol) (Option Symbol) CC-Launcher-Info)
  (lambda [argv debug? default-compilers force-lang]
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
            (cc-launcher-info lang biname subsystem entry default-compilers macros includes libpaths libraries))))))

(define cc-lang-from-extension : (->* (Path) (Bytes) Symbol)
  (lambda [native.cc [fallback-ext #".cpp"]]
    (string->symbol
     (string-downcase
      (bytes->string/utf-8
       (subbytes (or (path-get-extension native.cc) fallback-ext) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-destination-headers : (-> Path (Listof Path) Path (Listof (Pairof Path Path)))
  (lambda [source-rootdir includes dest-incdir]
    (for/fold ([ss : (Listof (Pairof Path Path)) null])
              ([header.h (in-list includes)])
      (define target-tail (find-relative-path source-rootdir header.h))
      (define spec : (Option (Pairof Path Path))
        (and (relative-path? target-tail)
             (let ([tails (explode-path target-tail)])
               (and (pair? tails)
                    (andmap path? tails)
                    (let ([target (apply build-path dest-incdir tails)])
                      (cons target header.h))))))
      
      (cond [(not spec) ss]
            [else (cons spec ss)]))))

(define cc-header-sed : (-> Path Path Void)
  (lambda [target source]
    (make-parent-directory* target)

    (parameterize ([current-custodian (make-custodian)])
      (define /dev/stdin (open-input-file source))
      (define /dev/stdout (open-output-file target #:exists 'truncate/replace))

      (dtrace-info #:topic 'misc "~a ~a ~a" (object-name cc-header-sed) source target)

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

(define cc-clear-destination-include : (-> Path (Listof (Pairof Path Path)) Void)
  (lambda [incdir0 headers]
    (when (directory-exists? incdir0)
      (define incdir (path->directory-path incdir0))
    
      (define deleting-files : (Listof Path)
        (for/list ([file (in-directory incdir)]
                   #:when (and (not (assoc file headers))
                               (or (file-exists? file)
                                   (null? (directory-list file #:build? #false)))))
          file))
      
      (for ([file (in-list deleting-files)]
            #:when (file-exists? file))
        (fg-rm 'misc file #:fr? #false))
      
      (for ([folder (in-list (remove-duplicates (filter-map path-only deleting-files)))])
        (let rmdir ([dir : Path folder])
          (unless (equal? incdir dir)
            (when (null? (directory-list dir #:build? #false))
              (define-values (base _ _?) (split-path dir))
              (fg-rm 'misc dir #:fr? #false)
              (when (path? base)
                (rmdir base)))))))))

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

(define cc-make-path->dest-path : (-> Path Native-Subpath (-> Path (Option (Pairof Path Path))))
  (lambda [destdir build-subpath]
    (define subdir (native-subpath->path build-subpath))
    (define drop-size (if (not subdir) 2 (+ (length (explode-path subdir)) 2)))
    
    (λ [[src : Path]] : (Option (Pairof Path Path))
      (define coms : (Option (Listof String)) (member "compiled" (map ~a (explode-path src))))
      
      (and (list? coms)
           (> (length coms) drop-size)
           (cons (apply build-path destdir (drop coms drop-size))
                 src)))))

(define cc-library-name : (-> Path-String String)
  (lambda [dylib.c]
    (let takeoff ([basename : Path (assert (file-name-from-path dylib.c))])
      (if (path-get-extension basename)
          (takeoff (path-replace-extension basename #""))
          (path->string basename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc #:phony make~release
                           #:desc "Build the collection as a C/C++ project [RELEASE]"))

(define cc-dbg-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'cc-dbg #:phony make~debug
                           #:desc "Build the collection as a C/C++ project [DEBUG]"))
