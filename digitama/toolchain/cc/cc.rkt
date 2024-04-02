#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require "../../minimal/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define object.ext : Bytes (if (eq? digimon-system 'windows) #".obj" #".o"))
(define binary.ext : String (if (eq? digimon-system 'windows) ".exe" ""))
(define library.ext : String (if (eq? digimon-system 'windows) ".lib" ".a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-compiler-candidates : (-> (Option (Listof Symbol)) (Listof Symbol))
  (lambda [compilers]
    (cond [(pair? compilers) compilers]
          [(eq? digimon-system 'macosx) '(clang gcc)]
          [(eq? digimon-system 'unix) '(gcc clang)]
          [else '(msvc gcc)])))

(define c-linker-candidates : (-> (Option (Listof Symbol)) (Listof Symbol))
  (lambda [linkers]
    (cond [(pair? linkers) linkers]
          [(eq? digimon-system 'macosx) '(clang gcc)]
          [(eq? digimon-system 'unix) '(gcc clang)]
          [else '(msvc gcc)])))

(define c-cpp-partner : (-> Symbol Symbol)
  (lambda [cc]
    (case cc
      [(gcc) 'g++]
      [(clang) 'clang++]
      [else cc])))

(define c-find-binary-path : (-> Symbol Symbol (Option Path))
  (lambda [basename topic]
    (find-executable-path (string-append (symbol->immutable-string basename) binary.ext) #false #false)))

(define c-include-file-path : (-> Path (Listof Path) String (Option Path))
  (lambda [dirname includes inc.h]
    (define nested.h (build-path dirname inc.h))

    (cond [(file-exists? nested.h) (simplify-path nested.h)]
          [else (for/or ([sub (in-list includes)])
                  (define nested.h (build-path dirname sub inc.h))
                  (and (file-exists? nested.h)
                       (simplify-path nested.h)))])))
