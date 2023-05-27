#lang typed/racket/base

(provide (all-defined-out))
(provide cc ld CC LD)

(require racket/list)
(require racket/symbol)

(require "digitama/toolchain/cc/cc.rkt")
(require "digitama/toolchain/cc/compiler.rkt")
(require "digitama/toolchain/cc/linker.rkt")
(require "digitama/toolchain/cc/configuration.rkt")

(require "digitama/toolchain/toolchain.rkt")
(require "digitama/exec.rkt")
(require "digitama/path.rkt")

(require "digitama/system.rkt")
(require "filesystem.rkt")
(require "dtrace.rkt")

; register toolchains
(require (submod "digitama/toolchain/bin/clang.rkt" register))
(require (submod "digitama/toolchain/bin/gcc.rkt" register))
(require (submod "digitama/toolchain/bin/msvc.rkt" register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-pick-compiler : (->* () ((Option (Listof Symbol))) (Option CC))
  (lambda [[compilers #false]]
    (ormap (λ [[compiler : Symbol]] (hash-ref cc-database compiler (λ [] #false)))
           (c-compiler-candidates compilers))))

(define c-compile : (->* (Path-String Path-String)
                         (#:cpp? Boolean #:verbose? Boolean #:debug? Boolean
                          #:includes (Listof C-Toolchain-Path-String) #:macros (Listof C-Compiler-Macro)
                          #:compilers (Option (Listof Symbol)))
                         Void)
  (lambda [#:cpp? [cpp? #false] #:verbose? [verbose? #false] #:debug? [debug? #false]
           #:includes [includes null] #:macros [macros null] #:compilers [compilers #false]
           infile outfile]
    (define compiler : (Option CC) (c-pick-compiler compilers))

    (unless (cc? compiler)
      (error 'c-compile "no suitable C compiler is found: ~a"
             (c-compiler-candidates compilers)))
      
    (make-parent-directory* outfile)
    (fg-recon-exec #:env (toolchain-env compiler)
                   'cc (if (not cpp?) (toolchain-program compiler) (cc-++ compiler))
                   (for/list : (Listof (Listof String)) ([layout (in-list (toolchain-option-layout compiler))])
                     (case layout
                       [(flags) ((cc-flags compiler) digimon-system cpp? null verbose? debug?)]
                       [(macros) ((cc-macros compiler) (cc-default-macros digimon-system cpp? debug?)
                                                       digimon-system cpp? (apply append (map c-macro-normalize macros)))]
                       [(includes) ((cc-includes compiler) (c-path-flatten includes) digimon-system cpp?)]
                       [(infile) ((cc-infile compiler) infile digimon-system cpp?)]
                       [(outfile) ((cc-outfile compiler) outfile digimon-system cpp?)]
                       [else (if (string? layout) (list layout) null)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-pick-linker : (->* () ((Option (Listof Symbol))) (Option LD))
  (lambda [[linkers #false]]
    (ormap (λ [[linker : Symbol]] (hash-ref ld-database linker (λ [] #false)))
           (c-linker-candidates linkers))))

(define c-link : (->* ((U Path-String (Listof Path-String)) Path-String)
                      (#:cpp? Boolean #:verbose? Boolean #:subsystem (Option Symbol) #:entry (Option Keyword)
                       #:libpaths (Listof C-Toolchain-Path-String) #:libraries (Listof C-Link-Library)
                       #:linkers (Option (Listof Symbol)))
                      Void)
  (lambda [#:cpp? [cpp? #false] #:verbose? [verbose? #false] #:subsystem [?subsystem #false] #:entry [?entry #false]
           #:libpaths [libpaths null] #:libraries [libs null] #:linkers [linkers #false]
           infiles outfile]
    (define linker : (Option LD) (c-pick-linker linkers))

    (unless (ld? linker)
      (error 'c-link "no suitable C linker is found: ~a"
             (c-linker-candidates linkers)))

    (make-parent-directory* outfile)
    (fg-recon-exec #:env (toolchain-env linker)
                   'ld (if (not cpp?) (toolchain-program linker) (ld-++ linker))
                   (for/list : (Listof (Listof String)) ([layout (in-list (toolchain-option-layout linker))])
                     (case layout
                       [(flags) ((ld-flags linker) digimon-system cpp? (not ?subsystem) null verbose? #false)]
                       [(ldflags) ((ld-flags linker) digimon-system cpp? (not ?subsystem) null verbose? #true)]
                       [(subsystem) ((ld-subsystem linker) digimon-system cpp? ?subsystem ?entry)]
                       [(libpath) ((ld-libpaths linker) (c-path-flatten libpaths) digimon-system cpp?)]
                       [(libraries) (let ([ld-lib (ld-libraries linker)])
                                      (apply append
                                             (for/list : (Listof (Listof String)) ([lib (in-list libs)])
                                               (cond [(symbol? lib) (ld-lib (list lib) #false digimon-system cpp?)]
                                                     [else (ld-lib (cdr lib) (car lib) digimon-system cpp?)]))))]
                       [(infiles) (cond [(path-string? infiles) ((ld-infile linker) infiles digimon-system cpp?)]
                                        [else (apply append (for/list : (Listof (Listof String)) ([f (in-list infiles)])
                                                              ((ld-infile linker) f digimon-system cpp?)))])]
                       [(outfile) ((ld-outfile linker) outfile digimon-system cpp?)]
                       [else (if (string? layout) (list layout) null)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-source->object-file : (->* (Path-String) ((Option Symbol) Boolean) (Option Path))
  (lambda [c [lang #false] [debug? #false]]
    (define basename : (Option Path) (file-name-from-path c))

    (and (path? basename)
         (build-path (native-rootdir/compiled c debug?)
                     (cond [(not lang) (path-replace-extension basename object.ext)]
                           [else (let ([lang.ext (format ".~a" (symbol->immutable-string lang))])
                                   (path-add-extension (path-replace-extension basename lang.ext) object.ext))])))))

(define c-source->shared-object-file : (->* (Path-String Boolean) ((Option String) Boolean) (Option Path))
  (lambda [c contained-in-package? [name #false] [debug? #false]]
    (define basename : (Option Path) (if (not name) (file-name-from-path c) (string->path name)))

    (and (path? basename)
         (let ([libname.so (path-replace-extension basename (system-type 'so-suffix))])
           (cond [(and contained-in-package?) (build-path (native-rootdir c) libname.so)]
                 [else (build-path (native-rootdir/compiled c debug?) libname.so)])))))

(define c-source->executable-file : (->* (Path-String Boolean) ((Option String) Boolean) (Option Path))
  (lambda [c contained-in-package? [name #false] [debug? #false]]
    (define basename : (Option Path) (if (not name) (file-name-from-path c) (string->path name)))

    (and (path? basename)
         (let ([bname (path-replace-extension basename binary.ext)])
           (cond [(and contained-in-package?) (build-path (native-rootdir c) bname)]
                 [else (build-path (native-rootdir/compiled c debug?) bname)])))))

(define c-include-headers : (->* (Path-String) ((Listof C-Toolchain-Path-String) #:check-source? Boolean #:topic Symbol) (Listof Path))
  (lambda [c [incdirs null] #:check-source? [recur? #false] #:topic [topic 'c-include-headers]]
    (define includes : (Listof Path) (filter relative-path? (c-path-flatten incdirs)))
    (let include.h ([entry : Path (if (string? c) (string->path c) c)]
                    [memory : (Listof Path) null])
      (define dirname : (Option Path) (path-only entry))
      (cond [(not dirname) memory]
            [else (foldl (λ [[include : Bytes] [memory : (Listof Path)]] : (Listof Path)
                           (define maybe-header : (Option (Pairof Bytes (Listof (Option Bytes)))) (regexp-match #px#"\"(.+?)\"" include))
                           (cond [(or (not maybe-header) (null? (cdr maybe-header)) (not (cadr maybe-header))) memory]
                                 [else (let* ([nbase.h (bytes->string/utf-8 (cadr maybe-header))]
                                              [nested.h (c-include-file-path dirname includes nbase.h)])
                                         (cond [(not nested.h) (dtrace-warning #:topic topic "including file not found: ~a in ~a" nbase.h entry) memory]
                                               [(member nested.h memory) memory]
                                               [else (let ([memory++ (include.h nested.h (append memory (list nested.h)))])
                                                       (cond [(not recur?) memory++]
                                                             [else ; for executables and shared obejcts
                                                              (let ([nested.c (c-header->maybe-source nested.h)])
                                                                (cond [(not nested.c) memory++]
                                                                      [else (include.h nested.c memory++)]))]))]))]))
                         memory
                         (call-with-input-file* entry
                           (λ [[/dev/stdin : Input-Port]]
                             ; TODO: implement a robust `#include` reader 
                             (regexp-match* #px"(?<=#include)\\s+[<\"].+?[\">]" /dev/stdin))))]))))

(define c-header->maybe-source : (->* (Path-String) ((Option (-> Path (Option Path)))) (Option Path))
  (lambda [h [src->file #false]]
    (for/or : (Option Path) ([ext (in-list (list #".c" #".cpp"))])
      (define h.c (path-replace-extension h ext))
      
      (and (file-exists? h.c)
           (cond [(not src->file) h.c]
                 [else (src->file h.c)])))))

(define c-headers->files : (->* ((Listof Path)) ((Option (-> Path (Option Path)))) (Listof Path))
  (lambda [deps [src->file #false]]
    (remove-duplicates
     (filter-map (λ [[h : Path]] (c-header->maybe-source h src->file))
                 deps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-list-compilers : (-> (Listof Symbol))
  (lambda []
    (hash-keys cc-database)))

(define c-list-linkers : (-> (Listof Symbol))
  (lambda []
    (hash-keys ld-database)))

(define c-toolchain-name : (-> Tool-Chain Symbol)
  (lambda [tc]
    (define basename : (Option Path) (file-name-from-path (toolchain-program tc)))
    
    (cond [(path? basename) (string->symbol (path->string (path-replace-extension basename #"")))]
          [else '|should not happen|])))
