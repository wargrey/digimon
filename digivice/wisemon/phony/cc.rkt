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
   [subsystem : Symbol]
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

(define make-depcc-specs : (-> (Listof CC-Launcher-Name) (Listof String) Wisemon-Specification)
  (lambda [launchers incdirs]
    (define-values (macros includes)
      (for/fold ([macros : (Listof C-Compiler-Macro) null]
                 [includes : (Listof C-Toolchain-Path-String) incdirs])
                ([launcher (in-list launchers)])
        (let ([info (cdr launcher)])
          (values (append macros (cc-launcher-info-macros info))
                  (append includes (cc-launcher-info-includes info))))))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([dep.c (in-list (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.c(pp)?$" file)) (current-directory)))])
      (define cpp-file? : Boolean (eq? (cc-lang-from-extension dep.c) 'cpp))
      (define deps.h : (Listof Path) (c-include-headers dep.c))
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
                   [else specs])))))

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
        (cons native.o (c-headers->files (c-include-headers native.c #:source-recursive? #true)
                                         (λ [[dep.c : Path]] (c-source->object-file dep.c lang)))))

      (list* (wisemon-spec native.o #:^ (cons native.c (c-include-headers native.c))
                           #:- (c-compile #:cpp? cpp? #:verbose? (compiler-verbose) #:macros (cc-launcher-info-macros info)
                                          #:includes (append incdirs (cc-launcher-info-includes info))
                                          native.c native.o))
             (wisemon-spec native #:^ objects
                           #:- (c-link #:cpp? cpp? #:verbose? (compiler-verbose) #:subsystem (cc-launcher-info-subsystem info)
                                       #:libpaths (cc-launcher-info-libpaths info) #:libraries (cc-launcher-info-libraries info)
                                       objects native))
             specs))))
    
(define make~cc : Make-Phony
  (lambda [digimon info-ref]
    (define incdirs : (Listof String) (list (path->string (digimon-path 'zone))))
    (define launchers : (Listof CC-Launcher-Name) (find-digimon-native-launcher-names info-ref))
    (define depcc-specs : Wisemon-Specification (make-depcc-specs launchers incdirs))
    (define cc-specs : Wisemon-Specification (make-cc-specs launchers incdirs))

    (wisemon-compile (current-directory) digimon info-ref)
    (wisemon-make (append depcc-specs cc-specs)
                  (let ([natives (wisemon-targets-flatten cc-specs)]
                        [specific-natives (current-make-real-targets)])
                    (cond [(null? specific-natives) natives]
                          [else (filter (λ [[n : Path]] (member n natives)) specific-natives)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-launcher-filter : (-> Any (Option CC-Launcher-Name))
  (lambda [native]
    (if (and (pair? native) (string? (car native)))
        (let ([native.cc (build-path (current-directory) (path-normalize/system (car native)))])
          (and (file-exists? native.cc)
               (cons native.cc (cc-filter-name (cdr native)))))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-filter-name : (-> Any CC-Launcher-Info)
  (lambda [argv]
    (let partition ([lang : (Option Symbol) #false]
                    [name : (Option String) #false]
                    [subsystem : Symbol 'CONSOLE]
                    [srehto : (Listof Any) null]
                    [options : (Listof Any) (if (list? argv) argv (list argv))])
      (if (pair? options)
          (let-values ([(opt rest) (values (car options) (cdr options))])
            (cond [(memq opt '(C c)) (partition (or lang 'c) name subsystem srehto rest)]
                  [(memq opt '(C++ c++ Cpp cpp)) (partition (or lang 'cpp) name subsystem srehto rest)]
                  [(memq opt '(windows desktop)) (partition lang name 'WINDOWS srehto rest)]
                  [(string? opt) (partition lang (or name opt) subsystem srehto rest)]
                  [else (partition lang name subsystem (cons opt srehto) rest)]))
          (let-values ([(macros includes libpaths libraries) (c-configuration-filter (reverse srehto) digimon-system)])
            (cc-launcher-info lang name subsystem macros includes libpaths libraries))))))

(define cc-lang-from-extension : (->* (Path) (Bytes) Symbol)
  (lambda [native.cc [fallback-ext #".cpp"]]
    (string->symbol
     (string-downcase
      (bytes->string/utf-8
       (subbytes (or (path-get-extension native.cc) fallback-ext) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-phony #:name 'cc #:phony make~cc
                      #:desc "Build the collection as a C/C++ project preprocessed by Racket"))
