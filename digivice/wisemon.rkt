#lang racket

(require raco/command-name)

(displayln (short-program+command-name))
(displayln (program+command-name))
(displayln (current-command-line-arguments))
