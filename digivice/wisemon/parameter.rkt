#lang typed/racket/base

(provide (all-defined-out))

(require racket/future)

(require typed/racket/unsafe)

(unsafe-require/typed/provide
 make
 [make-print-dep-no-line (Parameterof Boolean)]
 [make-print-checking (Parameterof Boolean)]
 [make-print-reasons (Parameterof Boolean)])

(unsafe-require/typed/provide
 setup/option
 [compiler-verbose (Parameterof Boolean)]
 [parallel-workers (Parameterof Nonnegative-Integer)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name 'wisemon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-make-real-targets : (Parameterof (Listof Path)) (make-parameter null))
(define current-make-phony-goal : (Parameterof (Option Symbol)) (make-parameter #false))

(define make-dry-run : (Parameterof Boolean) (make-parameter #false))
(define make-always-run : (Parameterof Boolean) (make-parameter #false))
(define make-just-touch : (Parameterof Boolean) (make-parameter #false))
(define make-trace-log : (Parameterof Boolean) (make-parameter #false))
(define make-errno : (Parameterof Byte) (make-parameter 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-restore-options! : (-> Void)
  (lambda []
    (parallel-workers (processor-count))
    (compiler-verbose #true)
    
    (make-set-verbose! #false)
    
    (make-dry-run #false)
    (make-always-run #false)
    (make-just-touch #false)
    (make-errno 1)))

(define make-set-verbose! : (-> Boolean Void)
  (lambda [switch]
    (for-each (λ [[make-verbose : (Parameterof Boolean)]]
                (make-verbose switch))
              (list make-print-dep-no-line make-print-checking
                    make-print-reasons make-trace-log))))
