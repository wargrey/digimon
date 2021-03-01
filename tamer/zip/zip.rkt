#lang typed/racket/gui

(provide main)

(require digimon/archive)

(require digimon/cmdopt)
(require digimon/date)

(require digimon/echo)
(require digimon/debug)
(require digimon/dtrace)
(require digimon/format)

(require digimon/digitama/exec)
(require digimon/digitama/bintext/zipinfo)

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
(define zip-exec : (-> Symbol Path-String * Void)
  (lambda [shname . argv]
    (define sh (find-executable-path (symbol->string shname)))

    (when (path? sh)
      (fg-recon-exec shname sh
                     (map (λ [[arg : Path-String]]
                            (list (if (string? arg) arg (path->string arg))))
                          argv)
                     'whatever))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip srcs) (λargv))

    (parameterize ([current-logger /dev/dtrace]
                   [date-display-format 'iso-8601])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (define-values (zipinfo:opts _) (parse-zipinfo-flags (list "-mht") #:help-output-port (current-output-port)))
                (define entries.λsh : (HashTable String (Listof String)) (make-hash))
                (define entries.zip : (HashTable String (Listof String)) (make-hash))
                
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

                (define target_zip : Path (path-add-extension target.zip #".zip"))
                
                (time** #:title 'λsh
                        (zip-create target.zip entries #:strategy strategy))

                (for ([e (in-list (zip-list-directories* target.zip))])
                  (hash-set! entries.λsh (zip-directory-filename e) (zip-entry-info e zipinfo:opts)))

                (time** #:title 'zip
                        (when (file-exists? target_zip)
                          (fg-recon-rm 'zip target_zip))
                        (apply zip-exec 'zip (format "-~aqr" (cdr strategy)) target_zip srcs))
                
                (for ([e (in-list (zip-list-directories* target_zip))])
                  (hash-set! entries.zip (zip-directory-filename e) (zip-entry-info e zipinfo:opts)))

                (define all-entries : (Listof (Listof String))
                  (for/fold ([merged-entries : (Listof (Listof String)) null])
                            ([(entry info) (in-hash entries.λsh)])
                    (define zip-info : (Listof String) (hash-ref entries.zip entry (inst list String)))
                    
                    (cond [(null? zip-info) (cons (cons "λsh" info) merged-entries)]
                          [else (cons (cons "λsh" info) (cons (cons "zip" zip-info) merged-entries))])))

                (when (pair? all-entries)
                  (let ([widths (text-column-widths all-entries)])
                    (for ([e (in-list all-entries)])
                      (define fgc (if (string=? (car e) "λsh") 'green 252))
                      (for ([col (in-list (cdr e))]
                            [wid (in-list (cdr widths))]
                            [idx (in-naturals)])
                        (when (> idx 0) (display #\space))

                        (let ([numerical? (memq (string-ref col (sub1 (string-length col))) (list #\% #\B))])
                          (echof (~a col #:min-width (+ wid 1) #:align (if numerical? 'right 'left)) #:fgcolor fgc)))
                      
                      (newline)))

                  (zip-display-tail-info target.zip 'green)
                  (zip-display-tail-info target_zip 252)))

              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-zip-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (zip-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
