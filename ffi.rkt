#lang racket/base

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe ffi/unsafe/define))

(require racket/path)

(require ffi/unsafe)
(require ffi/unsafe/define)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (digimon-ffi-lib stx)
  (syntax-parse stx #:literals []
    [(_ libname
        (~optional (~seq #:global? ?:expr) #:defaults ([? #'#true]))
        (~optional (~seq #:on-fail on-fail:expr) #:defaults ([on-fail #'#false])))
     #'(ffi-lib #:global? ? #:fail on-fail
                (let* ([rkt (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))]
                       [compiled (build-path (path-only rkt) "compiled" "native" (system-library-subpath #false) libname)]
                       [shipped (build-path (path-only rkt) (system-library-subpath #false) libname)])
                  (if (file-exists? compiled) compiled shipped)))]))
