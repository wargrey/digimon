#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)
(require racket/match)

(require "../shell.rkt")
(require "../unsafe/colorize.rkt")

(define shell~colorize : (-> Path Thread Any)
  (lambda [path env-thread]
    (drracket-colorize path (file->string path) env-thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define colorize-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'colorize #:shell shell~colorize
                      #:desc "Run with the DrRacket color lexer"))
