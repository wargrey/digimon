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
        (~alt (~optional (~seq #:global? global?:expr) #:defaults ([global? #'#true]))
              (~optional (~seq #:on-fail on-fail:expr) #:defaults ([on-fail #'#false]))
              (~optional (~seq #:subdir subdir:expr) #:defaults ([subdir #'#false])))
        ...)
     (syntax/loc stx
       (let ([modpath (variable-reference->module-source (#%variable-reference))]
             [libpath (system-library-subpath #false)])
         (if (not (path? modpath)) ; when distributed as a standalone executable
             (ffi-lib (build-path (ffi-distributed-library-path) libpath libname)
                      #:global? global?
                      #:fail (λ [] (ffi-lib #:global? global? #:fail on-fail
                                            (build-path libpath libname))))
             (ffi-lib libname
                      #:fail on-fail
                      #:global? global?
                      #:get-lib-dirs
                      (λ [] (list (native-rootdir/compiled modpath subdir)
                                  (native-rootdir modpath subdir)))))))]))

(define-syntax (digimon-ffi-obj stx)
  (syntax-parse stx #:literals []
    [(_ sym lib type)
     (syntax/loc stx
       (let* ([t type]
              [lazy (λ _ (get-ffi-obj sym lib t))])
         (get-ffi-obj sym lib t (λ [] lazy))))]))

(define-syntax (define-ffi-obj stx)
  (syntax-parse stx #:literals []
    [(_ sym:id (~optional #:in) lib (~optional #:as) type)
     (syntax/loc stx
       (define sym (digimon-ffi-obj 'sym lib type)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cpointer*?
  (lambda [v]
    (and v (cpointer? v))))
