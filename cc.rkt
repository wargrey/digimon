#lang typed/racket/base

(provide (all-defined-out))
(provide c-source-modelines)
(provide c-toolchain-program c-toolchain-option-layout)
(provide C-Tool-Chain)

(require racket/path)

(require "digitama/cc/compiler.rkt")
(require "digitama/cc/linker.rkt")
(require "digitama/cc/modeline.rkt")

(require "digitama/cc/cc.rkt")
(require "digitama/cc/exec.rkt")

(require "digitama/system.rkt")

; register toolchains
(require "digitama/cc/toolchain/gcc.rkt")
(require "digitama/cc/toolchain/msvc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-pick-compiler : (->* () ((Option (Listof Symbol))) (Option CC))
  (lambda [[compilers #false]]
    (ormap (λ [[compiler : Symbol]] (hash-ref cc-database compiler (λ [] #false)))
           (c-compiler-candidates compilers))))

(define c-compile : (-> Path-String Path-String [#:include-dirs (Listof Path-String)] [#:modelines (Listof C-Modeline)] [#:compilers (Option (Listof Symbol))] Void)
  (lambda [infile outfile #:include-dirs [includes null] #:modelines [modelines null] #:compilers [compilers #false]]
    (define compiler : (Option CC) (c-pick-compiler compilers))

    (if (cc? compiler)
        (c-toolchain-exec 'cc (c-toolchain-program compiler)
                          (for/list : (Listof (Listof String)) ([layout (in-list (c-toolchain-option-layout compiler))])
                            (case layout
                              [(flags) ((cc-flags compiler) digimon-system)]
                              [(macros) (append (cc-default-macros digimon-system) ((cc-macros compiler) digimon-system))]
                              [(includes) ((cc-includes compiler) digimon-system)]
                              [(infile) ((cc-infile compiler) infile digimon-system)]
                              [(outfile) ((cc-outfile compiler) outfile digimon-system)]
                              [else (if (string? layout) (list layout) null)]))
                          digimon-system)
        (error 'c-compile "no suitable C compiler is found: ~a"
               (c-compiler-candidates compilers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-pick-linker : (->* () ((Option (Listof Symbol))) (Option LD))
  (lambda [[linkers #false]]
    (ormap (λ [[linker : Symbol]] (hash-ref ld-database linker (λ [] #false)))
           (c-linker-candidates linkers))))

(define c-link : (-> (U Path-String (Listof Path-String)) Path-String [#:modelines (Listof C-Modeline)] [#:linkers (Option (Listof Symbol))] Void)
  (lambda [infiles outfile #:modelines [modelines null] #:linkers [linkers #false]]
    (define linker : (Option LD) (c-pick-linker linkers))

    (if (ld? linker)
        (c-toolchain-exec 'ld (c-toolchain-program linker)
                          (for/list : (Listof (Listof String)) ([layout (in-list (c-toolchain-option-layout linker))])
                            (case layout
                              [(flags) ((ld-flags linker) digimon-system)]
                              [(libpath) ((ld-libpaths linker) digimon-system)]
                              [(libraries) ((ld-libraries linker) (filter c:mdl:ld? modelines) digimon-system)]
                              [(infiles) (cond [(path-string? infiles) ((ld-infile linker) infiles digimon-system)]
                                               [else (apply append (for/list : (Listof (Listof String)) ([f (in-list infiles)])
                                                                     ((ld-infile linker) f digimon-system)))])]
                              [(outfile) ((ld-outfile linker) outfile digimon-system)]
                              [else (if (string? layout) (list layout) null)]))
                          digimon-system)
        (error 'c-link "no suitable C linker is found: ~a"
               (c-linker-candidates linkers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-object-destination : (-> Path-String Boolean (Option Path))
  (lambda [c contained-in-package?]
    (define dirname : (Option Path) (path-only c))
    (define basename : (Option Path) (file-name-from-path c))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     "native" (system-library-subpath #false)
                     (path-replace-extension basename object.ext)))))

(define c-library-destination : (-> Path-String Boolean (Option Path))
  (lambda [c contained-in-package?]
    (define dirname : (Option Path) (path-only c))
    (define basename : (Option Path) (file-name-from-path c))

    (and (path? dirname) (path? basename)
         (if contained-in-package?
             (build-path dirname (system-library-subpath #false)
                         (path-replace-extension basename (system-type 'so-suffix)))
             (build-path dirname (car (use-compiled-file-paths))
                         "native" (system-library-subpath #false)
                         (path-replace-extension basename (system-type 'so-suffix)))))))

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
                                               [else (include.h subsrc memory)]))]))
                         (append memory (list entry))
                         (call-with-input-file* entry
                           (λ [[/dev/stdin : Input-Port]]
                             (regexp-match* #px"(?<=#include )[<\"].+?.h[\">]" /dev/stdin))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-list-compilers : (-> (Listof Symbol))
  (lambda []
    (hash-keys cc-database)))

(define c-list-linkers : (-> (Listof Symbol))
  (lambda []
    (hash-keys ld-database)))

(define c-toolchain-name : (-> C-Tool-Chain Symbol)
  (lambda [tc]
    (define basename : (Option Path) (file-name-from-path (c-toolchain-program tc)))
    
    (cond [(path? basename) (string->symbol (path->string (path-replace-extension basename #"")))]
          [else '|should not happen|])))
