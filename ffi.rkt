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
     #'(let ([modpath (variable-reference->module-source (#%variable-reference))]
             [libpath (system-library-subpath #false)])
         (if (not (path? modpath)) ; when distributed as a standalone executable
             (ffi-lib #:global? ? #:fail on-fail (build-path libpath libname))
             (let ([this-root (path-only modpath)])
               (ffi-lib (build-path this-root "compiled" "native" libpath libname)
                        #:global? ? #:fail (λ [] (ffi-lib (build-path this-root libpath libname)
                                                          #:global? ? #:fail on-fail))))))]))
