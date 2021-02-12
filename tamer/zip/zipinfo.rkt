#lang typed/racket/gui

(provide (all-defined-out))

(require racket/path)
(require racket/symbol)

(require digimon/archive)
(require digimon/digitama/bintext/zipinfo)

(require digimon/cmdopt)
(require digimon/format)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option zipinfo-flags #: ZipInfo-Flags
  #:program 'zipinfo
  #:args [file.zip]

  #:once-each
  [[(#\A)                    "list all entries, including those not in the central directory"]

   [(#\h)                    "display the header line"]
   [(#\t)                    "display the trailer line"]
   [(#\z)                    "display file comment"]

   [(#\T)                    "print the file dates and times in the sortable decimal format"]
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

    (define file.zip : String (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [pretty-print-columns 160]
                   [current-command-line-arguments (vector)]
                   [date-display-format 'iso-8601])
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
  (lambda [opts file.zip]
    (define zip-entries : (U (Listof ZIP-Directory) (Listof ZIP-Entry))
      (cond [(zipinfo-flags-A opts) (zip-list-local-entries* file.zip)]
            [else (zip-list-directories* file.zip)]))

    (define zip-comments : (Pairof String (Listof (Pairof String String)))
      (cond [(zipinfo-flags-z opts) (zip-list-comments* file.zip)]
            [else (cons "" null)]))
    
    (when (zipinfo-flags-1 opts)
      (for-each displayln (zip-list zip-entries))
      (exit 0))

    (when (zipinfo-flags-h opts)
      (printf "Archive: ~a~n" (simple-form-path file.zip))
      (printf "Zip file size: ~a, number of entries: ~a~n"
              (~size (zip-content-size zip-entries)) (length zip-entries)))

    (define entries : (Listof (Listof String))
      (cond [(zipinfo-flags-2 opts) (map (inst list String) (zip-list zip-entries))]
            [else (for/list ([ze (in-list zip-entries)])
                    (filter string?
                            (if (zip-directory? ze)
                                (let-values ([(csize rsize) (values (zip-directory-csize ze) (zip-directory-rsize ze))])
                                  (list (zip-version (zip-directory-create-version ze)) (symbol->immutable-string (zip-directory-create-system ze))
                                        (zip-version (zip-directory-extract-version ze)) (symbol->immutable-string (zip-directory-extract-system ze))
                                        (zip-size rsize) (cond [(zipinfo-flags-m opts) (zip-cfactor csize rsize)] [(zipinfo-flags-l opts) (zip-size rsize)])
                                        (symbol->immutable-string (zip-directory-compression ze))
                                        (zip-datetime (zip-directory-mdate ze) (zip-directory-mtime ze) (zipinfo-flags-T opts))
                                        (zip-directory-filename ze)))
                                (let-values ([(csize rsize) (values (zip-entry-csize ze) (zip-entry-rsize ze))])
                                  (list (zip-version (zip-entry-extract-version ze)) (symbol->immutable-string (zip-entry-extract-system ze))
                                        (zip-size rsize) (cond [(zipinfo-flags-m opts) (zip-cfactor csize rsize)] [(zipinfo-flags-l opts) (zip-size rsize)])
                                        (symbol->immutable-string (zip-entry-compression ze))
                                        (zip-datetime (zip-entry-mdate ze) (zip-entry-mtime ze) (zipinfo-flags-T opts))
                                        (zip-entry-filename ze))))))]))

    (when (> (string-length (car zip-comments)) 0)
      (displayln (car zip-comments)))
    
    (when (pair? entries)
      (cond [(= (length (car entries)) 1) (for ([e entries]) (displayln (car e)))]
            [else (let ([widths (text-column-widths entries)])
                    (for ([e (in-list entries)])
                      (for ([col (in-list e)]
                            [wid (in-list widths)]
                            [idx (in-naturals)])
                        (when (> idx 0) (display #\space))

                        (let ([numerical? (memq (string-ref col (sub1 (string-length col))) (list #\% #\B))])
                          (display (~a col #:min-width (+ wid 1) #:align (if numerical? 'right 'left)))))

                      (let ([?comment (assoc (last e) (cdr zip-comments))])
                        (when (and ?comment (> (string-length (cdr ?comment)) 0))
                          (display #\space)
                          (display (cdr ?comment))))

                      (newline)))]))

    (when (zipinfo-flags-t opts)
      (define-values (csize rsize) (zip-content-size* file.zip))
      (printf "~a, ~a uncompressed, ~a compressed: ~a~n"
              (~n_w (length zip-entries) "entry") (~size rsize) (~size csize)
              (zip-cfactor csize rsize 1)))

    (exit 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-cfactor : (->* (Natural Natural) (Byte) String)
  (lambda [csize rsize [precision 0]]
    (~% #:precision (if (= precision 0) 0 `(= ,precision))
        (- 1 (if (= rsize 0) 1 (/ csize rsize))))))

(define zip-size : (-> Index String)
  (lambda [size]
    (~size size
           #:precision '(= 1)
           #:bytes->string (λ [n u] (~a n " B")))))

(define zip-version : (-> Index String)
  (lambda [version]
    (number->string (/ (real->double-flonum version) 10.0))))

(define zip-datetime : (-> Index Index Boolean String)
  (lambda [date time T?]
    (define the-date (seconds->date (msdos-datetime->utc-seconds date time #true) #true))

    (if (not T?)
        (date->string the-date #true)
        (string-append (number->string (date-year the-date)) (~0time (date-month the-date)) (~0time (date-day the-date))
                       "." (~0time (date-hour the-date)) (~0time (date-minute the-date)) (~0time (date-second the-date))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
