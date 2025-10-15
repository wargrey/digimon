#lang typed/racket/base

(provide (all-defined-out))

(require racket/future)
(require typed/racket/unsafe)

(unsafe-require/typed/provide
 setup/option
 [compiler-verbose (Parameterof Boolean)]
 [parallel-workers (Parameterof Positive-Integer)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-make-real-targets : (Parameterof (Listof Path)) (make-parameter null))
(define current-make-phony-goal : (Parameterof Symbol) (make-parameter 'all))

(define make-dry-run : (Parameterof Boolean) (make-parameter #false))
(define make-always-run : (Parameterof Boolean) (make-parameter #false))
(define make-just-touch : (Parameterof Boolean) (make-parameter #false))
(define make-trace-log : (Parameterof Boolean) (make-parameter #false))
(define make-verbose : (Parameterof Boolean) (make-parameter #false))
(define make-keep-going : (Parameterof Boolean) (make-parameter #false))
(define make-errno : (Parameterof Byte) (make-parameter 1))

(define make-assumed-oldfiles : (Parameterof (Listof Path)) (make-parameter null))
(define make-assumed-newfiles : (Parameterof (Listof Path)) (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-restore-options! : (-> Void)
  (lambda []
    (parallel-workers (processor-count))

    (make-trace-log #false)
    (make-set-verbose! #false)
    
    (make-dry-run #false)
    (make-always-run #false)
    (make-just-touch #false)
    (make-keep-going #false)
    (make-errno 1)

    (make-assumed-oldfiles null)
    (make-assumed-newfiles null)))

(define make-set-verbose! : (-> Boolean Void)
  (lambda [switch]
    (for-each (λ [[make-verbose : (Parameterof Boolean)]]
                (make-verbose switch))
              (list make-verbose compiler-verbose))))
