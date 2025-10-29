#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/match)

(require digimon/token)
(require digimon/cmdopt)

(require "../shell.rkt")
(require "../parameter.rkt")
(require "../unsafe/colorize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-shell : Symbol 'colorize)
(define the-desc : String "Run with the DrRacket color lexer")

(define-cmdlet-option colorize-flags #: Colorize-Flags
  #:program the-shell
  #:args [modpath.rkt]

  #:usage-help the-desc
  #:once-each
  [[(#\l lang)                                       lang          "replace the #lang line with ~1 for lexer"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-colorize : (-> Path (Option String) Thread Any)
  (lambda [path alt-lang env-thread]
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
(define shell~~colorize : (-> (Listof String) Thread Any)
  (lambda [argv env-thread]
    (define-values (options λargv) (parse-colorize-flags argv))

    (if (not (colorize-flags-help? options))
        (let ([path (cmdopt-string->path the-shell (λargv))])
          (shell-colorize path (colorize-flags-lang options) env-thread))
        (thread-send env-thread (display-colorize-flags #:exit #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define colorize-shell : Nanomon-Shell
  (nanomon-make-shell #:name the-shell
                      #:shell shell~~colorize
                      #:desc the-desc))
