#lang racket

(provide (all-defined-out))

(require ffi/unsafe)

(define ffi-distributed-library-path
  (let ([bin (build-path "bin")])
    (lambda []
      (define exec-file (find-system-path 'exec-file))
      (define orig-file (if (absolute-path? exec-file) exec-file (build-path (find-system-path 'orig-dir) exec-file)))
      (define this-dir (path-only orig-file))
      (define-values (parent-dir this-name dir?) (split-path this-dir))

      (build-path (if (equal? this-name bin) parent-dir this-dir) "lib"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-extern
  (lambda [variable ctype]
    (get-ffi-obj variable #false ctype)))

(define c-extern/enum
  ;;; racket->c can map multi names to one value, while c->racket uses the last name
  (lambda [symbols #:map-symbol [symmap string-downcase]]
    (_enum (foldl (lambda [c Es] (list* (string->symbol (symmap (~a c))) '= (get-ffi-obj c #false _ufixint) Es)) null symbols))))

(define c-extern/bitmask
  (lambda [symbols #:map-symbol [symmap string-downcase]]
    (_bitmask (foldl (lambda [c Bs] (list* (string->symbol (symmap (~a c))) '= (get-ffi-obj c #false _uint) Bs)) null symbols))))
