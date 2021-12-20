#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/path)

(require "../../../cc.rkt")

(require "../../../filesystem.rkt")
(require "../../../digitama/system.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Launcher-Info (List (Option Symbol) (Option String) Symbol (Listof Symbol)))
(define-type CC-Launcher-Name (Pairof Path CC-Launcher-Info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-native-launcher-names : (-> Info-Ref (Listof CC-Launcher-Name))
  (lambda [info-ref]
    (define maybe-launchers (info-ref 'native-launcher-names (λ [] null)))

    (unless (list? maybe-launchers)
      (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" maybe-launchers))

    (filter-map cc-launcher-filter maybe-launchers)))

(define make-depcc-specs : (-> (Listof String) Wisemon-Specification)
  (lambda [incdirs]
    (for/fold ([specs : Wisemon-Specification null])
              ([dep.c (in-list (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.c(pp)?$" file)) (current-directory)))])
      (define cpp-file? : Boolean (eq? (cc-lang-from-extension dep.c) 'cpp))
      (define incs.h : (Listof Path) (c-include-headers dep.c))
      (define dep++.o : Path (assert (c-source->object-file dep.c 'cpp)))
      
      (list* (wisemon-spec dep++.o #:^ (cons dep.c incs.h) #:- (c-compile dep.c dep++.o #:cpp? #true #:include-dirs incdirs))
             
             (cond [(not cpp-file?)
                    (let ([dep.o (assert (c-source->object-file dep.c 'c))])
                      (cons (wisemon-spec dep.o #:^ (cons dep.c incs.h) #:- (c-compile dep.c dep.o #:cpp? #false #:include-dirs incdirs))
                            specs))]
                   [else specs])))))

(define make-cc-specs : (-> Info-Ref (Listof String) Wisemon-Specification)
  (lambda [info-ref incdirs]
    (for/fold ([specs : Wisemon-Specification null])
              ([name (in-list (find-digimon-native-launcher-names info-ref))])
      (define native.c : Path (car name))
      (define lang : Symbol (or (cadr name) (cc-lang-from-extension native.c)))
      (define cpp? : Boolean (eq? lang 'cpp))
      (define native : Path (assert (c-source->executable-file native.c #false (caddr name))))
      (define subsystem : Symbol (cadddr name))
      (define flags : (Listof Symbol) (car (cddddr name)))

      (define native.o : Path (assert (c-source->object-file native.c lang)))
      (define deps.h : (Listof Path) (c-include-headers native.c))
      (define objects : (Listof Path) (cons native.o (c-headers->files deps.h (λ [[dep.c : Path]] (c-source->object-file dep.c lang)))))

      (list* (wisemon-spec native.o #:^ (cons native.c deps.h) #:- (c-compile native.c native.o #:cpp? cpp? #:include-dirs incdirs))
             (wisemon-spec native #:^ objects
                           #:- (c-link #:cpp? cpp? #:subsystem subsystem #:modelines (c-source-modelines native.c)
                                       objects native))
             specs))))
    
(define make~cc : Make-Phony
  (lambda [digimon info-ref]
    (define incdirs : (Listof String) (list (path->string (digimon-path 'zone))))
    (define depcc-specs : Wisemon-Specification (make-depcc-specs incdirs))
    (define cc-specs : Wisemon-Specification (make-cc-specs info-ref incdirs))
    
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
        (let ([native.cc (build-path (current-directory) (string->path/system (car native)))])
          (and (file-exists? native.cc)
               (cons native.cc (cc-filter-name (cdr native)))))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-filter-name : (-> Any CC-Launcher-Info)
  (lambda [argv]
    (let partition ([lang : (Option Symbol) #false]
                    [name : (Option String) #false]
                    [subsystem : Symbol 'CONSOLE]
                    [srehto : (Listof Symbol) null]
                    [options : (Listof Any) (if (list? argv) argv (list argv))])
      (cond [(null? options) (list lang name subsystem (reverse srehto))]
            [else (let-values ([(opt rest) (values (car options) (cdr options))])
                    (cond [(memq opt '(C c)) (partition (or lang 'c) name subsystem srehto rest)]
                          [(memq opt '(C++ c++ Cpp cpp)) (partition (or lang 'cpp) name subsystem srehto rest)]
                          [(memq opt '(windows desktop)) (partition lang name 'WINDOWS srehto rest)]
                          [(string? opt) (partition lang (or name opt) subsystem srehto rest)]
                          [(symbol? opt) (partition lang name subsystem (cons opt srehto) rest)]
                          [else (partition lang name subsystem srehto rest)]))]))))

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
