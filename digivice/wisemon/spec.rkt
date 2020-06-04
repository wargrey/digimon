#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../../wisemon.rkt"))

(require "parameter.rkt")

(require (except-in "../../wisemon.rkt" wisemon-make))
(require (rename-in "../../wisemon.rkt" [wisemon-make wisemon:make]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Specification) ((Listof Path-String) Boolean) Void)
  (lambda [specs [targets null] [always-run (make-always-run)]]
    (wisemon:make #:name the-name #:jobs (parallel-workers)
                  #:keep-going? (make-keep-going) #:dry-run? (make-dry-run) #:always-run? always-run #:just-touch? (make-just-touch)
                  #:assume-old (make-assume-oldfiles) #:assume-new (make-assume-newfiles)
                  specs targets)))
