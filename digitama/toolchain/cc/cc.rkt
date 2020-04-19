#lang typed/racket/base

(provide (all-defined-out))

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

(define c-find-binary-path : (-> Symbol (Option Path))
  (lambda [basename]
    (find-executable-path (string-append (symbol->string basename) binary.ext) #false #false)))
