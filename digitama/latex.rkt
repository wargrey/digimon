#lang typed/racket/base

(provide (all-defined-out))
(provide tex-renderer Tex-Renderer)

(require racket/path)
(require racket/file)
(require racket/port)
(require racket/string)

(require/typed
 racket/base
 [copy-file (-> Path-String Path-String Any Void)])

(require "typeset/renderer.rkt")
(require "typeset/tex.rkt")
(require "exec.rkt")

(require "system.rkt")

(require "../filesystem.rkt")
(require "../echo.rkt")

; register renders
(require "typeset/bin/pdftex.rkt")
(require "typeset/bin/xetex.rkt")
(require "typeset/bin/luahbtex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-render : (-> Symbol Path-String Path-String [#:fallback Symbol] [#:retry Byte] [#:disable-filter Boolean] Path)
  (lambda [renderer src.tex dest-dir #:fallback [fallback 'latex] #:retry [retry 4] #:disable-filter [disable-filter #true]]
    (define latex : (Option Tex-Renderer)
      (hash-ref latex-database renderer
                (λ [] (hash-ref latex-database fallback (λ [] #false)))))

    (cond [(not (tex-renderer? latex)) (error renderer "no such a tex renderer in ~a" (tex-list-renderers))]
          [(not (file-exists? src.tex)) (error renderer "no such a tex file: ~a" src.tex)]
          [else (let* ([preamble-filter (tex-renderer-preamble-filter latex)]
                       [post-exec (tex-renderer-post-exec latex)]
                       [TEXNAME (assert (file-name-from-path src.tex) path?)]
                       [TEXNAME.tex (build-path dest-dir TEXNAME)]
                       [TEXNAME.ext (build-path dest-dir (path-replace-extension TEXNAME (tex-renderer-extension latex)))]
                       [TEXNAME.log (build-path dest-dir (path-replace-extension TEXNAME #".log"))]
                       [same-file? (equal? src.tex TEXNAME.tex)]
                       [log-timestamp (file-mtime TEXNAME.log)])
                  (make-directory* dest-dir)

                  (cond [(or disable-filter (not preamble-filter)) (when (not same-file?) (copy-file src.tex TEXNAME.tex #true))]
                        [else (parameterize ([current-custodian (make-custodian)])
                                (echof #:fgcolor 'cyan "~a: ~a: ~a~n" renderer (object-name preamble-filter) src.tex)

                                (with-handlers ([exn:fail? (λ [[e : exn:fail]] (custodian-shutdown-all (current-custodian)) (raise e))])
                                  (define /dev/texin : Input-Port
                                    (cond [(not same-file?) (open-input-file src.tex)]
                                          [else (open-input-bytes (file->bytes src.tex))]))
                                  (define /dev/texout : Output-Port (open-output-file TEXNAME.tex #:exists 'replace))
                                  
                                  (let copy-filter ([fstatus : Any 'SOF])
                                    (unless (regexp-match-peek #px"^\\\\begin[{]document[}]" /dev/texin)
                                      (define src-line : (U String EOF) (read-line /dev/texin))
                                      (when (string? src-line)
                                        (define-values (maybe-line cstatus) (preamble-filter src-line fstatus))
                                        (cond [(string? maybe-line) (displayln maybe-line /dev/texout)]
                                              [(pair? maybe-line) (displayln (string-join maybe-line "\n") /dev/texout)])
                                        (unless (eq? cstatus 'EOF)
                                          (copy-filter cstatus)))))
                                  (copy-port /dev/texin /dev/texout))
                                
                                (custodian-shutdown-all (current-custodian)))])

                  (parameterize ([current-directory dest-dir])
                    (let rerun ([times : Natural 1])
                      (fg-exec (string->symbol (format "~a[~a]" renderer times))
                               (tex-renderer-program latex)
                               (list (list "-interaction=batchmode") (list (path->string TEXNAME.tex)))
                               digimon-system
                               (λ [op program status]
                                 (define log-now (file-mtime TEXNAME.log))
                                 (when (> log-now 0)
                                   (echof #:fgcolor 'yellow "~a: cat ~a~n" op TEXNAME.log)
                                   (call-with-input-file* TEXNAME.log
                                     (λ [[/dev/login : Input-Port]]
                                       (copy-port /dev/login (current-error-port)))))
                                 (when (<= log-now log-timestamp)
                                   (echof #:fgcolor 'yellow "~a: log has not updated~n" op))))

                      (when (and (file-exists? TEXNAME.log) (> (file-mtime TEXNAME.log) log-timestamp))
                        ;; see if we get a "Rerun" note, these seem to come in two flavors
                        ;; * Label(s) may have changed. Rerun to get cross-references right.
                        ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
                        (when (call-with-input-file* TEXNAME.log
                                (lambda [[/dev/login : Input-Port]]
                                  (regexp-match? #px#"changed\\.\\s+Rerun" /dev/login)))
                          (cond [(>= times retry) (raise-user-error renderer "could not get a stable result after ~a runs" times)]
                                [else (rerun (+ times 1))]))))

                    (cond [(not post-exec) TEXNAME.ext]
                          [else (post-exec renderer TEXNAME.ext digimon-system)])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-document-destination : (->* (Path-String) (Boolean) (Option Path))
  (lambda [tex [contained-in-package? #true]]
    (define dirname : (Option Path) (path-only tex))
    (define basename : (Option Path) (file-name-from-path tex))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     "typesettings" (path-replace-extension basename #".pdf")))))

(define tex-list-renderers : (-> (Listof Symbol))
  (lambda []
    (hash-keys latex-database)))
