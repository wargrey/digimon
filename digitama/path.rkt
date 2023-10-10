#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/list)
(require racket/symbol)
(require racket/string)

(require "system.rkt")
(require "../predicate.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Native-Subpath-Datum (U False String Symbol (Listof (U String Symbol))))

(define native-subpath-datum? : (-> Any Boolean : Native-Subpath-Datum)
  (lambda [v]
    (or (not v)
        (string-like? v)
        (and (list? v)
             ((inst andmap Any Boolean (U String Symbol))
              string-like? v)))))

(define native-subpath->path : (-> Native-Subpath-Datum (U Path-String False))
  (lambda [subnative]
    (cond [(not subnative) (system-library-subpath #false)]
          [(string? subnative) (native-subpath-component->path subnative)]
          [(symbol? subnative) (native-subpath-component->path subnative)]
          [else (let ([subpaths (filter-map native-subpath-component->path subnative)])
                  (and (pair? subpaths)
                       (apply build-path subpaths)))])))

(define native-subpath-component->path : (-> (U String Symbol) (U Path-String False))
  (lambda [subnative]
    (cond [(string? subnative) (and (> (string-length subnative) 0) subnative)]
          [else (case subnative
                  [(os os* system) (symbol->immutable-string digimon-system)]
                  [(subpath library) (system-library-subpath #false)]
                  [(word) (number->string (system-type 'word))]
                  [else #false])])))

(define native-shared-object-name-make : (-> Path-String Boolean String)
  (lambda [basename libname?]
    (define name.so : String (path->string (path-replace-extension basename (system-type 'so-suffix))))

    (cond [(not libname?) name.so]
          [else (string-append "lib" name.so)])))

(define native-shared-object-name-restore : (-> Path-String Boolean String)
  (lambda [libname.so libname?]
    (define libname : String (path->string (path-replace-extension libname.so #"")))

    (cond [(not libname?) libname]
          [(string-prefix? libname "lib") (substring libname 3)]
          [else libname])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define native-rootdir : (-> Path-String Native-Subpath-Datum Path)
  (lambda [src subnative]
    (define ntvdir (or (path-only src) (current-directory)))
    (define subpaths (native-subpath->path subnative))

    (cond [(not subpaths) ntvdir]
          [else (build-path ntvdir subpaths)])))

(define native-rootdir/compiled : (-> Path-String Boolean Native-Subpath-Datum Path)
  (lambda [src debug? subnative]
    (define ntvdir (or (path-only src) (current-directory)))
    (define subpaths (native-subpath->path subnative))
    (define native (if debug? (build-path "native" "debug") "native"))

    (cond [(not subpaths) (build-path ntvdir (car (use-compiled-file-paths)) native)]
          [else (build-path ntvdir (car (use-compiled-file-paths)) native subpaths)])))
