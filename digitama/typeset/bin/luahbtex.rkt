#lang typed/racket/base

(require "../tex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define luahbtex-preamble-filter : Tex-Preamble-Filter
  (lambda [line status]
    (cond [(regexp-match? #px"\\\\usepackage\\[utf8\\][{]inputenc[}]" line) (values #false 'encoded)]
          [(regexp-match? #px"\\\\usepackage\\[T1\\][{]fontenc[}]" line) (values "\\usepackage{fontspec}" 'fontset)]
          [(regexp-match? #px"\\\\newcommand[{]\\\\packageCJK[}]" line) (values #false 'commandset)]
          [(regexp-match? #px"\\\\packageCJK" line) (values #false 'EOF)]
          [else (values line status)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tex-register-renderer 'lualatex #:filter luahbtex-preamble-filter)
