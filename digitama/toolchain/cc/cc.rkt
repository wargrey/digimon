#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require "../toolchain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define object.ext : Bytes (if (eq? os 'windows) #".obj" #".o"))
(define binary.ext : String (if (eq? os 'windows) ".exe" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-compiler-candidates : (-> (Option (Listof Symbol)) (Listof Symbol))
  (lambda [compilers]
    (cond [(pair? compilers) compilers]
          [(eq? os 'macosx) '(clang gcc)]
          [(eq? os 'unix) '(gcc clang)]
          [else '(msvc)])))

(define c-linker-candidates : (-> (Option (Listof Symbol)) (Listof Symbol))
  (lambda [linkers]
    (cond [(pair? linkers) linkers]
          [(eq? os 'macosx) '(clang gcc)]
          [(eq? os 'unix) '(gcc clang)]
          [else '(msvc)])))

(define c-cpp-partner : (-> Symbol Symbol)
  (lambda [cc]
    (case cc
      [(gcc) 'g++]
      [(clang) 'clang++]
      [else cc])))

(define c-find-binary-path : (-> Symbol (Option Path))
  (lambda [basename]
    (find-executable-path (string-append (symbol->immutable-string basename) binary.ext) #false #false)))

(define c-include-file-path : (-> Path (Listof Path) String (Option Path))
  (lambda [dirname includes inc.h]
    (define nested.h (build-path dirname inc.h))

    (cond [(file-exists? nested.h) (simplify-path nested.h)]
          [else (for/or ([sub (in-list includes)])
                  (define nested.h (build-path dirname sub inc.h))
                  (and (file-exists? nested.h)
                       (simplify-path nested.h)))])))
