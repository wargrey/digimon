#lang typed/racket/base

(provide main)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zipcopy-flags #: ZipCopy-Flags
  #:program 'zipcopy
  #:args [dest.zip src.zip . sources]

  #:once-each
  [[(#\v)               #:=> zip-verbose
                        "run with verbose messages"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zipcopy-flags argument-list #:help-output-port (current-output-port)))
    (define-values (dest.zip src.zip entries) (λargv))
   
    (parameterize ([current-logger /dev/dtrace]
                   [date-display-format 'iso-8601])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (time** #:title 'zipcopy
                        (zip-copy src.zip entries dest.zip)))

              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
