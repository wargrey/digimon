#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

(require "../shell.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell~exec : (-> Path Thread Any)
  (lambda [path env-thread]
    (parameterize ([current-namespace (make-base-namespace)]
                   [global-port-print-handler pretty-print])
      (define main `(submod ,path main))
      
      (cond [(module-declared? main #true) (dynamic-require main 0)]
            [(dynamic-require path 0)]))
    
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'exec #:shell shell~exec
                      #:desc "Run lang's `main` submodule"))
