#lang typed/racket/base

(provide (all-defined-out))
(provide c-source-modelines)
(provide cc ld CC LD)

(require racket/path)
(require racket/file)

(require "digitama/toolchain/cc/cc.rkt")
(require "digitama/toolchain/cc/compiler.rkt")
(require "digitama/toolchain/cc/linker.rkt")
(require "digitama/toolchain/cc/modeline.rkt")

(require "digitama/toolchain/toolchain.rkt")
(require "digitama/exec.rkt")
(require "digitama/path.rkt")

(require "digitama/system.rkt")

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
                         (#:cpp? Boolean #:include-dirs (Listof Path-String) #:modelines (Listof C-Modeline) #:compilers (Option (Listof Symbol)))
                         Void)
  (lambda [infile outfile #:cpp? [cpp? #false] #:include-dirs [includes null] #:modelines [modelines null] #:compilers [compilers #false]]
    (define compiler : (Option CC) (c-pick-compiler compilers))

    (unless (cc? compiler)
      (error 'c-compile "no suitable C compiler is found: ~a"
             (c-compiler-candidates compilers)))

    (make-parent-directory* outfile)
    (fg-recon-exec 'cc (if (not cpp?) (toolchain-program compiler) (cc-++ compiler))
                   (for/list : (Listof (Listof String)) ([layout (in-list (toolchain-option-layout compiler))])
                     (case layout
                       [(flags) ((cc-flags compiler) digimon-system cpp?)]
                       [(macros) (append (cc-default-macros digimon-system cpp?) ((cc-macros compiler) digimon-system cpp?))]
                       [(includes) ((cc-includes compiler) includes digimon-system cpp?)]
                       [(infile) ((cc-infile compiler) infile digimon-system cpp?)]
                       [(outfile) ((cc-outfile compiler) outfile digimon-system cpp?)]
                       [else (if (string? layout) (list layout) null)]))
                   digimon-system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-pick-linker : (->* () ((Option (Listof Symbol))) (Option LD))
  (lambda [[linkers #false]]
    (ormap (λ [[linker : Symbol]] (hash-ref ld-database linker (λ [] #false)))
           (c-linker-candidates linkers))))

(define c-link : (->* ((U Path-String (Listof Path-String)) Path-String)
                      (#:cpp? Boolean #:modelines (Listof C-Modeline) #:linkers (Option (Listof Symbol)))
                      Void)
  (lambda [infiles outfile #:cpp? [cpp? #false] #:modelines [modelines null] #:linkers [linkers #false]]
    (define linker : (Option LD) (c-pick-linker linkers))

    (unless (ld? linker)
      (error 'c-link "no suitable C linker is found: ~a"
             (c-linker-candidates linkers)))

    (make-parent-directory* outfile)
    (fg-recon-exec 'ld (if (not cpp?) (toolchain-program linker) (ld-++ linker))
                   (for/list : (Listof (Listof String)) ([layout (in-list (toolchain-option-layout linker))])
                     (case layout
                       [(flags) ((ld-flags linker) digimon-system cpp?)]
                       [(libpath) ((ld-libpaths linker) digimon-system cpp?)]
                       [(libraries) (apply append (for/list : (Listof (Listof String)) ([mdl (in-list modelines)] #:when (c:mdl:ld? mdl))
                                                    ((ld-libraries linker) mdl digimon-system cpp?)))]
                       [(infiles) (cond [(path-string? infiles) ((ld-infile linker) infiles digimon-system cpp?)]
                                        [else (apply append (for/list : (Listof (Listof String)) ([f (in-list infiles)])
                                                              ((ld-infile linker) f digimon-system cpp?)))])]
                       [(outfile) ((ld-outfile linker) outfile digimon-system cpp?)]
                       [else (if (string? layout) (list layout) null)]))
                   digimon-system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-source->object-file : (-> Path-String (Option Path))
  (lambda [c]
    (define basename : (Option Path) (file-name-from-path c))

    (and (path? basename)
         (build-path (native-rootdir/compiled c)
                     (path-replace-extension basename object.ext)))))

(define c-source->shared-object-file : (-> Path-String Boolean (Option Path))
  (lambda [c contained-in-package?]
    (define basename : (Option Path) (file-name-from-path c))

    (and (path? basename)
         (let ([libname.so (path-replace-extension basename (system-type 'so-suffix))])
           (cond [(and contained-in-package?) (build-path (native-rootdir c) libname.so)]
                 [else (build-path (native-rootdir/compiled c) libname.so)])))))

(define c-include-headers : (-> Path-String (Listof Path))
  (lambda [c]
    (let include.h ([entry : Path (if (string? c) (string->path c) c)]
                    [memory : (Listof Path) null])
      (define dirname : (Option Path) (path-only entry))
      (cond [(not dirname) memory]
            [else (foldl (λ [[include : Bytes] [memory : (Listof Path)]] : (Listof Path)
                           (define maybe-header : (Option (Pairof Bytes (Listof (Option Bytes)))) (regexp-match #px#"\"(.+?)\"" include))
                           (cond [(or (not maybe-header) (null? (cdr maybe-header)) (not (cadr maybe-header))) memory]
                                 [else (let ([subsrc (simplify-path (build-path dirname (bytes->string/utf-8 (cadr maybe-header))))])
                                         (cond [(member subsrc memory) memory]
                                               [(file-exists? subsrc) (include.h subsrc (append memory (list subsrc)))]
                                               [else #| the including files might already been commented out |# memory]))]))
                         memory
                         (call-with-input-file* entry
                           (λ [[/dev/stdin : Input-Port]]
                             ; TODO: implement a formal `#include` reader 
                             (regexp-match* #px"(?<=#include)\\s+[<\"].+?.h(pp)?[\">]" /dev/stdin))))]))))

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
