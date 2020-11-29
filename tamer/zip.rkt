#lang typed/racket/gui

(provide main)

(require racket/path)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/format)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zipinfo-flags #: ZipInfo-Flags
  #:program 'zipinfo
  #:args [file.zip . entry]

  #:once-each
  [[(#\v)   #:=> zip-verbose "run with verbose messages"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zipinfo-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip entries) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)]
                   [date-display-format 'iso-8601])
      (exit (time-apply* (λ [] (let ([tracer (thread (make-zip-log-trace))])
                                 (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                                   (cond [(null? entries) (zip-extract file.zip)]
                                         [else (let-values ([(_ rest-entries unknowns) (zip-extract* file.zip entries)])
                                                 (length unknowns))]))
                                 (dtrace-datum-notice eof)
                                 (thread-wait tracer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
