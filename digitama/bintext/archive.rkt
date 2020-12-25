#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)
(require racket/symbol)

(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define archive-stdin-permission : Nonnegative-Fixnum #o000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct archive-entry
  ([src : (U Path Bytes)]
   [name : Path-String]
   [ascii? : Boolean]
   [methods : (Listof Symbol)]
   [options : (Listof Any)]
   [utc-time : (Option Integer)]
   [permission : Nonnegative-Fixnum]
   [comment : (Option String)])
  #:type-name Archive-Entry
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-archive-file-entry : (->* ((U Bytes String Path))
                                       ((Option String) #:ascii? Boolean #:methods (Listof Symbol) #:options (Listof Any) #:comment (Option String))
                                       Archive-Entry)
  (lambda [src [name #false] #:ascii? [ascii? #true] #:methods [methods null] #:options [options null] #:comment [comment #false]]
    (define path : Path
      (simple-form-path
       (cond [(path? src) src]
             [(string? src) (string->path src)]
             [else (bytes->path src)])))
    
    (archive-entry path (or name (path->string path)) ascii? methods options
                   (file-or-directory-modify-seconds path) (file-or-directory-permissions path 'bits)
                   comment)))

(define make-archive-ascii-entry : (->* ((U Bytes String))
                                        ((Option Path-String) #:methods (Listof Symbol) #:options (Listof Any)
                                                              #:utc-time (Option Integer) #:permission Nonnegative-Fixnum #:comment (Option String))
                                        Archive-Entry)
  (lambda [#:utc-time [mtime #false] #:permission [permission archive-stdin-permission] #:methods [methods null] #:options [options null] #:comment [comment #false]
           src [name #false]]
    (archive-entry (if (string? src) (string->bytes/utf-8 src) src)
                   (or name (symbol->immutable-string (gensym 'ascii)))
                   #true methods options mtime permission
                   comment)))

(define make-archive-binary-entry : (->* ((U Bytes String))
                                         ((Option Path-String) #:methods (Listof Symbol) #:options (Listof Any)
                                                               #:utc-time (Option Integer) #:permission Nonnegative-Fixnum #:comment (Option String))
                                         Archive-Entry)
  (lambda [#:utc-time [mtime #false] #:permission [permission archive-stdin-permission] #:methods [methods null] #:options [options null] #:comment [comment #false]
           src [name #false]]
    (archive-entry (if (string? src) (string->bytes/utf-8 src) src)
                   (or name (symbol->immutable-string (gensym 'binary)))
                   #false methods options mtime permission
                   comment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define archive-port-name : (-> Input-Port String)
  (lambda [/dev/zipin]
    (define name (object-name /dev/zipin))

    (cond [(path? name) (path->string name)]
          [(string? name) name]
          [(symbol? name) (symbol->immutable-string name)]
          [else (format "~a" name)])))

(define archive-entry-reroot : (->* (Path-String (Option Path-String) (Option Path-String)) ((Option Symbol)) String)
  (lambda [name root zip-root [gen-stdin-name #false]]
    (define stdin? : Boolean (equal? name ""))
    
    (cond [(and stdin? (not gen-stdin-name)) ""]
          [else (let* ([name (if (not stdin?) name (symbol->immutable-string (gensym gen-stdin-name)))]
                       [rpath (cond [(relative-path? name) name]
                                   [(not (path-string? root)) (find-root-relative-path name)]
                                   [else (find-relative-path (simple-form-path root) (simplify-path name)
                                                             #:more-than-root? #false ; relative to root
                                                             #:more-than-same? #false ; build "." path, docs seems to be wrong
                                                             #:normalize-case? #false)])])
                  (cond [(path-string? zip-root) (some-system-path->string (build-path (find-root-relative-path zip-root) rpath))]
                        [(path-for-some-system? rpath) (some-system-path->string rpath)]
                        [else rpath]))])))

(define archive-suffix-regexp : (-> (Listof Symbol) Regexp)
  (lambda [suffixes]
    (regexp (string-join (map symbol->immutable-string (remove-duplicates suffixes)) "|"
                         #:before-first ".(" #:after-last ")$"))))
