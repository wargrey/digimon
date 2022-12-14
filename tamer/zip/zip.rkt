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
(require digimon/digitama/bintext/zip)
(require digimon/digitama/bintext/archive)
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
  #:args [file.zip source . sources]

  #:once-each
  [[(#\s strategy)         #:=> zip-strategy#level strategy level #: (Pairof Symbol Byte)
                           "run with the strategy ~1 and compression level ~2"]
   [(#\r recurse-paths)    "travel the directory recursively"]
   [(#\t temporary)        "create temporarily"]
   [(#\p progress)         #:=> zip-progress
                           "show progress bars"]
   [(#\M memory-level)     #:=> cmdopt-string+>byte memlevel    #: Positive-Byte
                           "encoding in memory level of ~1"]

   [(#\n)                  #:=> cmdopt-string->index span  #: Index
                           "test with only first ~1 bytes"]
   [(#\o)                  #:=> cmdopt-string->index skip  #: Natural
                           "skip ~1 bytes before testing"]
   
   [(#\C change-directory) #:=> cmdopt-string->path root #: Path
                           "set root directory for entry path to ~1"]
   [(#\l)                  #:=> cmdopt-string->symbol locale #: Symbol
                           "force encoding filename with ~1"]
   [(64)                   #:=> zip-64bit
                           "force building zip64 file"]
   [(#\i)                  #:=> zip-interoperate
                           "interoperate with `zip` application"]
   [(#\v)                  #:=> zip-verbose
                           "run with verbose messages"]]
  #:multi
  [[(#\c huffman-codes)    #:=> cmdopt-string->symbol huffman-codes #: Symbol
                           "specific the huffman codes (fixed, dynamic, or auto)"]])

(define zip-64bit : (Parameterof Boolean) (make-parameter #false))
(define zip-progress : (Parameterof Boolean) (make-parameter #false))
(define zip-interoperate : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-exec : (-> Symbol Path-String * Void)
  (lambda [shname . argv]
    (define sh (find-executable-path (symbol->string shname)))

    (when (path? sh)
      (fg-recon-exec shname sh
                     (map (λ [[arg : Path-String]]
                            (list (if (string? arg) arg (path->string arg))))
                          argv)))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip source rest) (λargv))
    (define sources (cons source rest))

    (parameterize ([current-logger /dev/dtrace]
                   [date-display-format 'iso-8601]
                   [default-stdin-locale (zip-flags-l options)]
                   [default-stdout-locale (zip-flags-l options)]
                   [current-directory (or (zip-flags-change-directory options) (current-directory))])
      (exit (let ([tracer (thread (make-zip-log-trace))])
              (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (define-values (zipinfo:opts _) (parse-zipinfo-flags (list "-lht") #:help-output-port (current-output-port)))
                (define entries.λsh : (HashTable String (Listof String)) (make-hash))
                (define entries_zip : (HashTable String (Listof String)) (make-hash))
                
                (define tempdir : Path (build-path (find-system-path 'temp-dir) "lambda-sh"))
                (define strategy : (Pairof Symbol Index) (or (zip-flags-strategy options) (cons 'default 6)))
                (define all-huffcodes : (Listof Symbol) (zip-flags-huffman-codes options))

                (define span : Index (or (zip-flags-n options) 0))
                (define skip : Natural (or (zip-flags-o options) 0))

                (define entries : Archive-Entries
                  (for/list : (Listof (U Archive-Entry Archive-Entries)) ([huffcode (if (null? all-huffcodes) (in-value 'auto) (in-list all-huffcodes))])
                    (for/list : (Listof (U Archive-Entry Archive-Entries)) ([src (in-list (map string->path sources))])
                      (define alias : (Option Path-String) (if (relative-path? src) (current-directory) ""))
                      (cond [(zip-flags-recurse-paths options) (make-archive-directory-entries src alias #:configure #false #:options (list huffcode))]
                            [(= skip span 0) (make-archive-file-entry src (archive-entry-reroot src alias #false) #:options (list huffcode))]
                            [else (let ([/dev/stdin (open-input-file src)])
                                    (when (> skip 0) (drop-bytes /dev/stdin skip))
                                    (make-archive-binary-entry (cond [(= span 0)  (port->bytes /dev/stdin)]
                                                                     [else (let ([bs (read-bytes span /dev/stdin)])
                                                                             (if (eof-object? bs) #"" bs))])
                                                               (archive-entry-reroot src alias #false) #:options (list huffcode)))]))))
                
                (define target.zip : Path
                  (if (zip-flags-temporary options)
                      (build-path tempdir (or (file-name-from-path file.zip) file.zip))
                      (simple-form-path file.zip)))

                (define target_zip : Path (path-add-extension target.zip #".zip"))

                (when (file-exists? target_zip)
                  (fg-recon-rm 'zip target_zip))           

                (when (zip-progress)
                  [default-archive-entry-progress-handler (make-archive-entry-terminal-gauge)]
                  [default-archive-progress-handler (make-archive-terminal-gauge #:at (cons 1 128))])

                (time** #:title 'λsh
                        (zip-create #:root (zip-flags-change-directory options) #:strategy strategy #:force-zip64? (zip-64bit)
                                    #:memory-level (or (zip-flags-memory-level options) 8)
                                    target.zip entries))

                (for ([e (in-list (zip-directory-list* target.zip))])
                  (hash-set! entries.λsh (zip-directory-filename e) (zip-entry-info e zipinfo:opts)))

                (when (zip-flags-i options)
                  (time** #:title 'zip
                          (apply zip-exec 'zip
                                 (format "-~aq~a" (cdr strategy) (if (zip-flags-recurse-paths options) 'r ""))
                                 target_zip sources)))

                (when (file-exists? target_zip)
                  (for ([e (in-list (zip-directory-list* target_zip))])
                    (hash-set! entries_zip (zip-directory-filename e) (zip-entry-info e zipinfo:opts))))

                (define all-entries : (Listof (Listof String))
                  (append (for/fold ([merged-entries : (Listof (Listof String)) null])
                                    ([(entry info) (in-hash entries.λsh)]
                                     #:when (not (zip-folder-name? entry)))
                            (define zip-info : (Listof String) (hash-ref entries_zip entry (inst list String)))
                            
                            (cond [(null? zip-info) (cons (cons "λsh" info) merged-entries)]
                                  [else (list* (cons "λsh" info) (cons "zip" zip-info) merged-entries)]))
                          (for/list : (Listof (Listof String))
                            ([(entry zip-info) (in-hash entries_zip)]
                             #:when (not (or (zip-folder-name? entry)
                                             (hash-has-key? entries.λsh entry))))
                            (cons "zip" zip-info))))
                
                (when (pair? all-entries)
                  (let ([widths (text-column-widths all-entries)])
                    (for ([e (in-list all-entries)])
                      (define fgc (if (string=? (car e) "λsh") 'green 252))
                      (for ([col (in-list (cdr e))]
                            [wid (in-list (cdr widths))]
                            [idx (in-naturals)])
                        (when (> idx 0)
                          (display #\space))

                        (let ([numerical? (memq (string-ref col (sub1 (string-length col))) (list #\% #\B))])
                          (echof (~a col #:min-width (+ wid 1) #:align (if numerical? 'right 'left)) #:fgcolor fgc)))
                      
                      (newline)))

                  (zip-display-tail-info target.zip 'green)
                  (when (file-exists? target_zip)
                    (zip-display-tail-info target_zip 252))))

              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (main (current-command-line-arguments)))
