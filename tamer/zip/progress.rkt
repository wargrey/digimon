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
(port-count-lines-enabled #false)

(define chars-width : Index 64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define update-gauge : (-> Symbol String Natural Natural Boolean Void)
  (let ([last-count : (Boxof Integer) (box 0)]
        [bar : String (make-string 1 #\space)])
    (lambda [topic entry-name zipped total finish-entry?]
      (define % : Flonum (real->double-flonum (if (>= zipped total) 1.0 (/ zipped total))))
      (define count : Integer (exact-floor (* % chars-width)))
      (define lcount : Integer (unbox last-count))

      (when (> count lcount)
        (set-box! last-count count)

        (when (= lcount 0)
          (display "["))

        (for ([i (in-range lcount count)])
          (echof bar #:bgcolor 'green))

        (esc-save)
        (when (< count chars-width)
          (esc-move-right (- chars-width count)))
        (printf "] [~a%] ~a" (~r (* % 100.0) #:precision '(= 2)) entry-name)
        (esc-restore)

        (flush-output))

      (when (and finish-entry?)
        (set-box! last-count 0)
        (newline)))))

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
  (parameterize ([default-archive-entry-progress-handler update-gauge])
    (main (current-command-line-arguments))))
  