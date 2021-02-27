#lang typed/racket/gui

(provide main)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/date)

(require digimon/debug)
(require digimon/dtrace)

(require (except-in "zipinfo.rkt" main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-strategy#level : (-> Symbol String String (Pairof Symbol Byte))
  (lambda [option str.name str.level]
    (define strategy : Symbol (cmdopt-string->symbol option str.name))
    (define level : Byte (cmdopt-string->byte option str.level))

    (cons strategy level)))

(define-cmdlet-option zip-flags #: Zip-Flags
  #:program 'zip
  #:args [file.zip . sources]

  #:once-each
  [[(#\s strategy)      #:=> zip-strategy#level strategy level #: (Pairof Symbol Byte)
                        "run with the strategy ~1 and compression level ~2"]
   [(#\r recurse-paths) "travel the directory recursively"]
   [(#\t temporary)     "create temporarily"]
   [(#\v)               #:=> zip-verbose
                        "run with verbose messages"]])

(define zip-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zipinfo:main : (-> (U (Listof String) (Vectorof String)) Any)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zipinfo-flags argument-list #:help-output-port (current-output-port)))
    (define file.zip : String (λargv))

    (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
      (zipinfo options file.zip))))
  
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip srcs) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [date-display-format 'iso-8601])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (define tempdir : Path (build-path (find-system-path 'temp-dir) "lambda-sh"))
                (define sources : (Listof Path) (map simple-form-path srcs))
                (define strategy : (Pairof Symbol Index) (or (zip-flags-strategy options) (cons 'default 6)))
                (define entries : Archive-Entries
                  (for/list : (Listof (U Archive-Entry Archive-Entries)) ([src (in-list sources)])
                    (if (zip-flags-recurse-paths options)
                        (make-archive-directory-entries src #:configure #false)
                        (make-archive-file-entry src))))
                (define target.zip : Path
                  (if (zip-flags-temporary options)
                      (build-path tempdir (or (file-name-from-path file.zip) file.zip))
                      (simple-form-path file.zip)))
                
                (time** (zip-create target.zip entries #:strategy strategy))
                (zipinfo:main (list "-mht" (path->string target.zip)))

                (let ([zip (find-executable-path "zip")]
                      [zipinfo (find-executable-path "zipinfo")]
                      [target.zip.zip (path-add-extension target.zip #".zip")])
                  (when (path? zip)
                    (newline)
                    (apply system*/exit-code zip (format "-~aqr" (cdr strategy)) target.zip.zip sources)
                    (when (path? zipinfo)
                      (system*/exit-code zipinfo "-t" (path->string target.zip.zip))))))

              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
