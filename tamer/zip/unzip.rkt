#lang typed/racket/gui

(provide main)

(require digimon/archive)
(require digimon/stdio)

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
  [[(#\b)                                                "hexdump file content in binary mode"]
   [(#\w)   #:=> cmdopt-string+>byte width     #: Byte   "hexdump file content in ~1 bytes per line"]
   [(#\v)   #:=> unzip-verbose                           "run with verbose messages"]
   [(#\t)   #:=> unzip-check                             "check only"]
   [(#\p)   #:=> unzip-progress                          "show progress bars"]
   [(#\l)   #:=> cmdopt-string-identity locale #: String "set default locale to ~1"]])

(define unzip-verbose : (Parameterof Boolean) (make-parameter #false))
(define unzip-check : (Parameterof Boolean) (make-parameter #false))
(define unzip-progress : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-unzip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip entries) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)]
                   [date-display-format 'iso-8601]
                   [default-stdin-locale (or (unzip-flags-l options) 'utf-8)])
      (exit (time* (let ([tracer (thread (make-zip-log-trace))])
                     (begin0 (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                               (if (unzip-check)
                                   (let ()
                                     (when (unzip-progress)
                                       [default-archive-entry-progress-handler (make-archive-entry-terminal-gauge #:final-char #\space #:overlay-name? #true)]
                                       [default-archive-progress-handler (make-archive-terminal-gauge #:at 128)])
                                     (cond [(null? entries) (zip-verify file.zip)]
                                           [else (let-values ([(failures rest-entries unknowns) (zip-verify* file.zip entries)])
                                                   (+ failures (length unknowns)))]))
                                   
                                   (let ([hexdump (make-archive-hexdump-reader
                                                   #:binary? (unzip-flags-b options)
                                                   #:width (or (unzip-flags-w options)
                                                               (if (unzip-flags-b options) 16 32)))])
                                     (cond [(null? entries) (zip-extract file.zip hexdump)]
                                           [else (let-values ([(_ rest-entries unknowns) (zip-extract* file.zip entries hexdump)])
                                                   (length unknowns))]))))
                             
                             (dtrace-datum-notice eof)
                             (thread-wait tracer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (unzip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
