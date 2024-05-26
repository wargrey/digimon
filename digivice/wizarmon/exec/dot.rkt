#lang typed/racket/base

(provide (all-defined-out))

(require "../../../digitama/exec.rkt")
(require "../../../digitama/diagram/graphviz.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-dot : (-> Path Symbol Bytes Any)
  (lambda [path.gv -T extension]
    (define dot.ext (assert (gv-script-destination path.gv extension #true)))

    (gv-render path.gv -T #:outfile dot.ext)
    (fg-recon-open-file 'exec dot.ext)))
