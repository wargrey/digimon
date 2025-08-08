#lang racket/base

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))
(provide (all-from-out ffi/unsafe/alloc))

(provide (all-from-out '#%foreign))
(provide (rename-out [ctype-c->scheme ctype-c->racket]
                     [ctype-scheme->c ctype-racket->c]))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require racket/unsafe/ops)

(require (only-in '#%foreign
                  ctype-basetype
                  ctype-c->scheme
                  ctype-scheme->c))

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
              (~optional (~seq #:subdir subdir:expr) #:defaults ([subdir #'#false]))
              (~optional (~seq #:custodian cust:expr) #:defaults ([cust #'#false])))
        ...)
     (syntax/loc stx
       (let ([modpath (variable-reference->module-source (#%variable-reference))]
             [libpath (system-library-subpath #false)])
         (if (not (path? modpath)) ; when distributed as a standalone executable
             (ffi-lib (build-path (ffi-distributed-library-path) libpath libname)
                      #:global? global?
                      #:custodian cust
                      #:fail (λ [] (ffi-lib #:global? global? #:fail on-fail #:custodian cust
                                            (build-path libpath libname))))
             (ffi-lib libname
                      #:fail on-fail
                      #:global? global?
                      #:custodian cust
                      #:get-lib-dirs
                      (λ [] (list (native-rootdir/compiled modpath subdir)
                                  (native-rootdir modpath subdir)))))))]))

(define-syntax (define-ffi-parameter stx)
  (syntax-parse stx #:literals []
    [(_ sym:id (~optional #:in) lib (~optional #:as) type)
     (syntax/loc stx
       (define sym (make-c-parameter 'sym lib type)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define _char  (make-ctype _byte  char->integer integer->char))
(define _uchar (make-ctype _ubyte char->integer integer->char))
(define _any_string (make-ctype _string (λ [v] (if (string? v) v (format "~a" v))) values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define _take_ram_snapshot_t  (_fun _string -> _void))
(define _register_variable_t (_fun _string _symbol _uintptr _symbol -> _void))
(define _register_array_t    (_fun _string _symbol _uintptr _symbol _size -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &
  (lambda [ptr]
    (cast ptr _pointer _uintptr)))

(define %p
  (case-lambda
    [(ptr) (let ([addr (cond [(cpointer? ptr) (& ptr)]
                             [(exact-integer? ptr) ptr]
                             [else (& (ffi-obj-ref ptr #false))])])
             (string-append "0x" (number->string addr 16)))]
    [(sym dylib) (%p (ffi-obj-ref sym dylib))]))

(define ram-step
  (lambda [ptr type [maybe-ram #false]]
    (define ram
      (cond [(bytes? maybe-ram) ram]
            [(byte? maybe-ram) (make-bytes maybe-ram)]
            [else (make-bytes (ctype-sizeof type))]))

    (memmove ram 0 ptr 0 1 type)
    (values ram
            (ptr-ref ptr type)
            (ptr-add ptr 1 type))))

(define ram-step!
  (lambda [ptr type [maybe-ram #false]]
    (define ram
      (cond [(bytes? maybe-ram) ram]
            [(byte? maybe-ram) (make-bytes maybe-ram)]
            [else (make-bytes (ctype-sizeof type))]))

    (memmove ram 0 ptr 0 1 type)
    (values ram
            (ptr-ref ptr type)
            (ptr-add! ptr 1 type))))

(define ram-step*
  (lambda [ptr type ram &datum]
    (define count
      (if (vector? &datum)
          (let ([size (unsafe-vector-length &datum)])
            (for ([idx (in-range size)])
              (unsafe-vector-set! &datum idx (ptr-ref ptr type idx)))
            size)
          (begin (unsafe-set-box! &datum (ptr-ref ptr type)) 1)))
    
    (memmove ram 0 ptr 0 count type)
    (ptr-add ptr count type)))

(define ram-step*!
  (lambda [ptr type ram &datum]
    (define count
      (if (vector? &datum)
          (let ([size (unsafe-vector-length &datum)])
            (for ([idx (in-range size)])
              (unsafe-vector-set! &datum idx (ptr-ref ptr type idx)))
            size)
          (begin (unsafe-set-box! &datum (ptr-ref ptr type)) 1)))
    
    (memmove ram 0 ptr 0 count type)
    (ptr-add! ptr count type)))

(define ram-step-for-bytes
  (lambda [ptr type [count 1]]
    (define size (ctype-sizeof type))
    (define ram (make-bytes (* size count)))
    (memmove ram 0 ptr 0 count type)
    (values ram (ptr-add ptr count type))))

(define ram-step-for-bytes!
  (lambda [ptr type [count 1]]
    (define size (ctype-sizeof type))
    (define ram (make-bytes (* size count)))
    (memmove ram 0 ptr 0 count type)
    (values ram (ptr-add! ptr count type))))

(define ram-step-for-datum
  (lambda [ptr type]
    (values (ptr-ref ptr type)
            (ptr-add ptr 1 type))))

(define ram-step-for-datum!
  (lambda [ptr type]
    (values (ptr-ref ptr type)
            (ptr-add! ptr 1 type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cpointer*?
  (lambda [v]
    (and v (cpointer? v))))

(define make-ctype/release
  (lambda [ctype deallocator]
    (define basetype (ctype-basetype ctype))
    (define racket->c (ctype-scheme->c ctype))
    (define c->racket (ctype-c->scheme ctype))

    (define (wrap datum)
      ((deallocator (λ [] datum))))

    (make-ctype (or basetype ctype) racket->c
                (λ [c] (wrap (if c->racket (c->racket c) c))))))

(define make-ctype*
  (lambda [ctype out-hook [in-hook #false]]
    (define basetype (ctype-basetype ctype))
    (define racket->c (ctype-scheme->c ctype))
    (define c->racket (ctype-c->scheme ctype))

    (define (ctype-in-hook rkt)
      (define v (in-hook rkt))
      (if (void? v) rkt v))
    
    (define (ctype-out-hook rkt)
      (define v (out-hook rkt))
      (if (void? v) rkt v))

    (make-ctype (or basetype ctype)
                (cond [(not in-hook) racket->c]
                      [(not racket->c) in-hook]
                      [else (λ [rkt] (ctype-in-hook rkt))])
                (cond [(not out-hook) c->racket]
                      [(not c->racket) ctype-out-hook]
                      [else (λ [c] (ctype-out-hook (c->racket c)))]))))

(define ctype-bind-box
  (lambda [ctype &dest]
    (unless (box? &dest)
      (raise-argument-error
       'ctype-bind-box "box?" &dest))
    
    (make-ctype* ctype
                 (λ [r] (unsafe-set-box! &dest r)))))
