#lang typed/racket/gui

(provide main)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/format)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zip-flags #: Zip-Flags
  #:program 'zipinfo
  #:args [file.zip . entry]

  #:once-each
  [[(#\b)                                          "hexdump file content in binary mode"]
   [(#\w)   #:=> cmdopt-string+>byte width #: Byte "hexdump file content in ~1 bytes per line"]
   [(#\v)   #:=> zip-verbose                       "run with verbose messages"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip entries) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)]
                   [date-display-format 'iso-8601])
      (exit (time* (let ([tracer (thread (make-zip-log-trace))])
                     (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                       (define hexdump
                         (make-archive-hexdump-reader
                          #:binary? (zip-flags-b options)
                          #:width (or (zip-flags-w options)
                                      (if (zip-flags-b options) 16 32))))
                       
                       (cond [(null? entries) (zip-extract file.zip hexdump)]
                             [else (let-values ([(_ rest-entries unknowns) (zip-extract* file.zip entries hexdump)])
                                     (length unknowns))]))
                     (dtrace-datum-notice eof)
                     (thread-wait tracer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
