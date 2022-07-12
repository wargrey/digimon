#lang typed/racket

(provide (all-defined-out))

(require digimon/archive)
(require digimon/stdio)

(require digimon/cmdopt)
(require digimon/date)

(require digimon/echo)
(require digimon/debug)
(require digimon/dtrace)
(require digimon/format)

(require digimon/digitama/exec)
(require digimon/digitama/bintext/archive)
(require digimon/digitama/bintext/zipinfo)

(require (except-in "zipinfo.rkt" main))
(require (except-in "zip.rkt" main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip source rest) (λargv))
    (define sources (cons source rest))
    (define LC_ALL (or (zip-flags-l options) 'GB18030))
    (define fgc '252)

    (parameterize ([current-logger /dev/dtrace]
                   [date-display-format 'iso-8601]
                   [default-stdin-locale LC_ALL]
                   [default-stdout-locale LC_ALL])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (define-values (zipinfo:opts _) (parse-zipinfo-flags (list "-lht") #:help-output-port (current-output-port)))
                (define entries.λsh : (HashTable String (Listof String)) (make-hash))
                
                (define tempdir : Path (build-path (find-system-path 'temp-dir) "lambda-sh"))
                (define strategy : (Pairof Symbol Index) (or (zip-flags-strategy options) (cons 'default 6)))
                
                (define entries : Archive-Entries
                  (for/list : (Listof (U Archive-Entry Archive-Entries)) ([src (in-list (map string->path sources))])
                    (if (zip-flags-recurse-paths options)
                        (make-archive-directory-entries src #:configure #false)
                        (make-archive-file-entry src))))
                
                (define target.zip : Path
                  (if (zip-flags-temporary options)
                      (build-path tempdir (or (file-name-from-path file.zip) file.zip))
                      (simple-form-path file.zip)))

                (when (zip-progress)
                  [default-archive-entry-progress-handler (make-archive-entry-terminal-gauge)]
                  [default-archive-progress-handler (make-archive-terminal-gauge #:at (cons 2 0))])
                
                (time** #:title 'λsh
                        (zip-create target.zip entries #:root (zip-flags-change-directory options) #:strategy strategy #:force-zip64? (zip-64bit)))

                (for ([e (in-list (zip-directory-list* target.zip))])
                  (hash-set! entries.λsh (zip-directory-filename e) (zip-entry-info e zipinfo:opts)))

                (define all-entries : (Listof (Listof String))
                  (for/fold ([merged-entries : (Listof (Listof String)) null])
                            ([(entry info) (in-hash entries.λsh)])
                    (cons (cons "λsh" info) merged-entries)))

                (when (pair? all-entries)
                  (let ([widths (text-column-widths all-entries)])
                    (for ([e (in-list all-entries)])
                      (for ([col (in-list (cdr e))]
                            [wid (in-list (cdr widths))]
                            [idx (in-naturals)])
                        (when (> idx 0) (display #\space))

                        (let ([numerical? (memq (string-ref col (sub1 (string-length col))) (list #\% #\B))])
                          (echof (~a col #:min-width (+ wid 1) #:align (if numerical? 'right 'left)) #:fgcolor fgc)))
                      
                      (newline)))

                  (zip-display-tail-info target.zip fgc)))

              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
