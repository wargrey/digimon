#lang typed/racket/gui

(provide main)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/format)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option unzip-flags #: UnZip-Flags
  #:program 'unzip
  #:args [file.zip . entry]

  #:once-each
  [[(#\b)                                          "hexdump file content in binary mode"]
   [(#\w)   #:=> cmdopt-string+>byte width #: Byte "hexdump file content in ~1 bytes per line"]
   [(#\v)   #:=> zip-verbose                       "run with verbose messages"]
   [(#\t)   #:=> zip-check                         "check only"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))
(define zip-check : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (do-unzip stx)
  (syntax-case stx []
    [(_ file.zip entries unzip-expr)
     (syntax/loc stx
       (let ([unzip unzip-expr])
         (cond [(null? entries) (zip-extract file.zip unzip)]
               [else (let-values ([(_ rest-entries unknowns) (zip-extract* file.zip entries unzip)])
                       (length unknowns))])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-unzip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip entries) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)]
                   [date-display-format 'iso-8601])
      (exit (time* (let ([tracer (thread (make-zip-log-trace))])
                     (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                       (if (zip-check)
                           (do-unzip file.zip entries
                                     (make-archive-verification-reader #:dtrace 'unzip))
                           
                           (do-unzip file.zip entries
                                     (make-archive-hexdump-reader
                                               #:binary? (unzip-flags-b options)
                                               #:width (or (unzip-flags-w options)
                                                           (if (unzip-flags-b options) 16 32))))))
                     
                     (dtrace-datum-notice eof)
                     (thread-wait tracer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
