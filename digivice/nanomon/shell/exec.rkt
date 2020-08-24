#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/match)

(require "../shell.rkt")
(require "../unsafe/colorize.rkt")

(define shell~exec : (-> Path Thread Any)
  (lambda [path env-thread]
    (parameterize ([current-namespace (make-base-namespace)])
      (dynamic-require `(submod ,path main) 0))
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'exec #:shell shell~exec
                      #:desc "Run lang's `main` submodule"))
