#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

(require racket/path)

(require ffi/unsafe)

(define-syntax (digimon-ffi-lib stx)
  (syntax-parse stx #:literals []
    [(_ libname
        (~optional (~seq #:global? ?:expr) #:defaults ([? #'#true]))
        (~optional (~seq #:on-fail on-fail:expr) #:defaults ([on-fail #'#false])))
     #'(ffi-lib #:global? ? #:fail on-fail
                (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
                            "compiled" "native" (system-library-subpath #false) libname))]))
