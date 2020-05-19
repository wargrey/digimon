#lang typed/racket/base

(require "../tex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define luahbtex-preamble-filter : Tex-Preamble-Filter
  (lambda [line status]
    (cond [(regexp-match? #px"\\\\documentclass" line) (values (list line "\\usepackage{fontspec}") 'EOF)]
          [else (values line status)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tex-register-renderer 'lualatex #:filter luahbtex-preamble-filter)
