#lang racket/base

(provide (all-defined-out))
(provide (all-from-out dynext))

(require racket/path)
(require racket/file)
(require racket/bool)
(require racket/list)
(require racket/string)

(require racket/match)
(require racket/format)
(require racket/function)

(require setup/dirs)

(require dynext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-compiler
  (lambda []
    (define cc (current-extension-compiler))
    
    (and (path? cc)
         (dyn-binary-name cc))))

(define c-linker
  (lambda []
    (define ld (current-extension-linker))
    
    (and (path? ld)
         (dyn-binary-name ld))))

(define c-object-destination
  (lambda [c contained-in-package?]
    (build-path (path-only c) (car (use-compiled-file-paths))
                "native" (system-library-subpath #false)
                (append-object-suffix (extract-base-filename/c (file-name-from-path c))))))

(define c-library-destination
  (lambda [c contained-in-package?]
    (if contained-in-package?
        (build-path (path-only c) (system-library-subpath #false)
                    (path-replace-extension (file-name-from-path c) (system-type 'so-suffix)))
        (build-path (path-only c) (car (use-compiled-file-paths))
                    "native" (system-library-subpath #false)
                    (path-replace-extension (file-name-from-path c) (system-type 'so-suffix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-include-paths
  (lambda [cc system]
    (append (case cc
              [(cl) (dyn-msvc-include)]
              [else null])
            (case system
              [(macosx) (list "/usr/local/include")]
              [else null]))))

(define c-compiler-flags
  (lambda [cc system]
    (list* (format "-D__~a__" system)
           (case cc
             [(gcc) (list "-std=c11" "-m64")]
             [else null]))))

(define c-linker-flags
  (lambda [c ld system]
    (define -shared
      (append (case ld
                [(ld) (list "-shared")]
                [(cl) (map (λ [lib] (format "/LIBPATH:~a" lib)) (dyn-msvc-library))]
                [else null])
              (case system
                [(macosx) (list "-L/usr/local/lib")]
                [(illumos) (list "-m64")]
                [else null])))
    (for/fold ([ldflags -shared])
              ([line (in-list (file->lines c))] #:when (regexp-match? #px"#include\\s+<" line))
      (define modeline (regexp-match #px".+ld:(\\w+)?:?([^*]+)(\\*/)?$" line))
      (cond [(not modeline) ldflags #| TODO: deal with "-L" and `pkg-config` |#]
            [else (match-let ([(list _ hint ld _) modeline])
                    (for/fold ([ld-++ ldflags])
                              ([flags (in-port read (open-input-string ld))]
                               #:when (pair? flags) #| filter out empty list |#)
                      (match (cons system (and hint (map string->symbol (string-split hint ":"))))
                        [(list 'macosx 'framework) ; /* ld:framework: (IOKit) */
                         (append ld-++ (let ([-fw (list (~a #\- hint))])
                                         (add-between (map ~a flags) -fw #:splice? #true #:before-first -fw)))]
                        [(cons _ (or (? false?) (? (curry memq system)))) ; /* ld: (ssh2) or ld:illumos: (kstat) */
                         (append ld-++ (map (curry ~a "-l") flags))]
                        [_ ldflags])))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (include.h entry [memory null])
  (foldl (λ [include memory]
           (cond [(regexp-match #px#"<.+?>" include)
                  => (λ [header] memory)]
                 [(regexp-match #px#"\"(.+?)\"" include)
                  => (λ [header]
                       (let ([subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 (cadr header))))])
                         (cond [(member subsrc memory) memory]
                               [else (include.h subsrc memory)])))]))
         (append memory (list entry))
         (call-with-input-file* entry
           (λ [/dev/stdin]
             (regexp-match* #px"(?<=#include )[<\"].+?.h[\">]" /dev/stdin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bindir-path (build-path "bin"))

(define (dyn-binary-name bin)
  (string->symbol (path->string (path-replace-extension (file-name-from-path bin) #""))))

(define (dynext-msvc-root+arch)
  (define cc (current-extension-compiler))
  (and (path? cc)
       (let-values ([(parent arch dir?) (split-path (path-only cc))]
                    [(bin) (build-path "bin")])
         (let search-root ([dir parent])
           (and (path? dir)
                (let-values ([(parent name dir?) (split-path dir)])
                  (cond [(equal? name bindir-path) (cons parent arch)]
                        [else (search-root parent)])))))))

(define (dyn-msvc-include)
  (define root+arch (dynext-msvc-root+arch))
  (cond [(not root+arch) null]
        [else (list (build-path (car root+arch) "include"))]))

(define (dyn-msvc-library)
  (define root+arch (dynext-msvc-root+arch))
  (cond [(not root+arch) null]
        [else (list (build-path (car root+arch) "lib" (cdr root+arch)))]))
