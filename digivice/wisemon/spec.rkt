#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../../wisemon.rkt"))

(require racket/file)
(require racket/path)
(require racket/format)

(require "parameter.rkt")

(require (except-in "../../wisemon.rkt" wisemon-make))
(require (rename-in "../../wisemon.rkt" [wisemon-make wisemon:make]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Specification) ((Listof Path-String)) Void)
  (lambda [specs [targets null]]
    (wisemon:make #:name the-name #:jobs (parallel-workers)
                  #:keep-going? (make-keep-going) #:dry-run? (make-dry-run) #:always-run? (make-always-run) #:just-touch? (make-just-touch)
                  #:assume-old (make-assume-oldfiles) #:assume-new (make-assume-newfiles)
                  specs targets)))
