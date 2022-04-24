#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/match)

(require "../shell.rkt")
(require "../parameter.rkt")
(require "../unsafe/colorize.rkt")

(require "../../../token.rkt")

(define shell~colorize : (-> Path Thread Any)
  (lambda [path env-thread]
    (define alt-lang : (Option String) (nanomon-lang))

    (define content : String
      (call-with-input-file* path
        (λ [[/dev/langin : Input-Port]]
          (cond [(not alt-lang) (port->string /dev/langin)]
                [else (syn-token-port-skip-lang-line /dev/langin)
                      (string-append (format "#lang ~a" alt-lang)
                                     (string #\newline #\newline)
                                     (port->string /dev/langin))]))))
    
    (drracket-colorize path content env-thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define colorize-shell : Nanomon-Shell
  (nanomon-make-shell #:name 'colorize #:shell shell~colorize
                      #:desc "Run with the DrRacket color lexer"))
