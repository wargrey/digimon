#lang typed/racket/base

(provide (all-defined-out))

(require digimon/digitama/exec)
(require digimon/digitama/diagram/graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-dot : (-> Path Symbol Bytes Any)
  (lambda [path.gv -T extension]
    (define dot.ext (assert (gv-script-destination path.gv extension #false)))

    (gv-render path.gv -T #:outfile dot.ext)
    (fg-recon-open-file 'exec dot.ext)))
