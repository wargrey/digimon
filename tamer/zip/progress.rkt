#lang typed/racket/base

(require digimon/archive)
(require digimon/dtrace)
(require digimon/debug)
(require digimon/echo)

(require digimon/digitama/bintext/archive)

(require racket/path)
(require racket/math)
(require racket/date)
(require racket/format)

(require "zip.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Void)
  (lambda [argument-list]
    (define-values (options λargv) (parse-zip-flags argument-list #:help-output-port (current-output-port)))
    (define-values (file.zip source rest) (λargv))
    (define sources (cons source rest))

    (call-with-dtrace
        (λ [] (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false))])
                (define tempdir : Path (build-path (find-system-path 'temp-dir) "lambda-sh"))
                (define strategy : (Pairof Symbol Index) (or (zip-flags-strategy options) (cons 'default 6)))
                
                (define entries : Archive-Entries
                  (for/list : (Listof (U Archive-Entry Archive-Entries)) ([src (in-list sources)])
                    (define rootdir : (Option Path-String) (if (relative-path? src) (current-directory) ""))
                    (if (zip-flags-recurse-paths options)
                        (make-archive-directory-entries src rootdir #:configure #false)
                        (make-archive-file-entry src (archive-entry-reroot src rootdir #false)))))

                (define target.zip : Path
                  (if (zip-flags-temporary options)
                      (build-path tempdir (or (file-name-from-path file.zip) file.zip))
                      (simple-form-path file.zip)))
                
                (time** #:title 'λsh
                        (zip-create target.zip entries #:strategy strategy #:force-zip64? (zip-64bit))
                        (newline)))))))

(module+ main
  (parameterize ([default-archive-entry-progress-handler (make-archive-entry-terminal-gauge)])
    (main (current-command-line-arguments))))
  