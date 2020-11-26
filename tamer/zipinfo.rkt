#lang typed/racket/gui

(provide main)

(require racket/path)

(require digimon/archive)
(require digimon/cmdopt)
(require digimon/format)

(require digimon/debug)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zipinfo-flags #: ZipInfo-Flags
  #:program 'zipinfo
  #:args [file.zip]

  #:once-each
  [[(#\A)                    "list all entries, including those not in the central directory"]

   [(#\h)                    "list the header line"]
   [(#\t)                    "list the trailer line"]
   [(#\z)                    "display file comment"]
   [(#\v)   #:=> zip-verbose "run with verbose messages"]]

  #:once-any
  [[(#\1)                    "list filenames only"]
   [(#\2)                    "list filenames, but allow other information"]
   [(#\s)                    "list zip info with short format"]
   [(#\m)                    "list zip info with medium format"]
   [(#\l)                    "list zip info with long format"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zipinfo-flags argument-list #:help-output-port (current-output-port)))

    (define file.zip : String (car (λargv)))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)])
      (exit (time-apply* (λ [] (let ([tracer (thread (make-zip-log-trace))])
                                 (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                                   (zipinfo options file.zip))
                                 (dtrace-datum-notice eof)
                                 (thread-wait tracer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

(define zipinfo : (-> ZipInfo-Flags String Any)
  (lambda [options file.zip]
    (define zip-entries : (U (Listof ZIP-Directory) (Listof ZIP-Entry))
      (cond [(zipinfo-flags-A options) (zip-list-local-entries* file.zip)]
            [else (zip-list-directories* file.zip)]))
    
    (when (zipinfo-flags-1 options)
      (for-each displayln (zip-list zip-entries))
      (exit 0))

    (when (zipinfo-flags-h options)
      (printf "Archive: ~a~n" (simple-form-path file.zip))
      (printf "Zip file size: ~a, number of entries: ~a~n"
              (~size (zip-content-size zip-entries)) (length zip-entries)))

    (define entries : (Listof (Listof String))
      (cond [(zipinfo-flags-2 options) (map (inst list String) (zip-list zip-entries))]
            [(zipinfo-flags-s options) (map (inst list String) (zip-list zip-entries))]
            [else null]))

    (when (pair? entries)
      (cond [(= (length (car entries)) 1) (for-each displayln entries)]
            [else (for-each displayln entries)]))

    (when (zipinfo-flags-t options)
      (define-values (csize rsize) (zip-content-size* file.zip))
      (printf "~a, ~a uncompressed, ~a compressed: ~a~n"
              (~n_w (length zip-entries) "entry") (~size rsize) (~size csize)
              (if (= rsize 0) "0%" (zip-compression-factor csize rsize))))

    (exit 0)))

(define zip-compression-factor : (-> Natural Natural String)
  (lambda [csize rsize]
    (~% (- 1 (/ csize rsize)) #:precision '(= 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
