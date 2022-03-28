#lang racket/base

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))
(provide (all-from-out ffi/unsafe/alloc))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require "digitama/ffi.rkt")
(require "digitama/path.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (digimon-ffi-lib stx)
  (syntax-parse stx #:literals []
    [(_ libname
        (~optional (~seq #:global? ?:expr) #:defaults ([? #'#true]))
        (~optional (~seq #:on-fail on-fail:expr) #:defaults ([on-fail #'#false])))
     (syntax/loc stx
       (let ([modpath (variable-reference->module-source (#%variable-reference))]
             [libpath (system-library-subpath #false)])
         (if (not (path? modpath)) ; when distributed as a standalone executable
             (ffi-lib (build-path (ffi-distributed-library-path) libpath libname)
                      #:fail (λ [] (ffi-lib (build-path libpath libname) #:global? ? #:fail on-fail))
                      #:global? ?)
             (ffi-lib libname
                      #:get-lib-dirs (λ [] (list (native-rootdir/compiled modpath) (native-rootdir modpath)))
                      #:fail on-fail
                      #:global? ?))))]))
