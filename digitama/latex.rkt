#lang typed/racket/base

(provide (all-defined-out))
(provide tex-renderer Tex-Renderer)

(require racket/file)
(require racket/port)
(require racket/string)

(require "exec.rkt")
(require "typeset/renderer.rkt")
(require "typeset/exec.rkt")
(require "typeset/tex.rkt")

(require "../filesystem.rkt")
(require "../dtrace.rkt")

; register renders
(require "typeset/bin/pdftex.rkt")
(require "typeset/bin/xetex.rkt")
(require "typeset/bin/euptex.rkt")
(require "typeset/bin/luahbtex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-render : (->* (Symbol Path-String Path-String) (#:fallback Symbol #:retry Byte #:enable-filter Boolean) Path)
  (lambda [renderer src.tex dest-dir #:fallback [fallback 'latex] #:retry [retry 4] #:enable-filter [enable-filter #false]]
    (define latex : (Option Tex-Renderer)
      (hash-ref latex-database renderer
                (λ [] (hash-ref latex-database fallback (λ [] #false)))))

    (cond [(not (tex-renderer? latex)) (error renderer "no such a tex renderer in ~a" (tex-list-renderers))]
          [(not (file-exists? src.tex)) (error renderer "no such a tex file: ~a" src.tex)]
          [else (let* ([preamble-filter (tex-renderer-preamble-filter latex)])
                  (if (and enable-filter preamble-filter)
                      (let ([TEXNAME.tex (build-path dest-dir (assert (file-name-from-path src.tex) path?))])
                        (parameterize ([current-custodian (make-custodian)])
                          
                          (fg-recon-mkdir renderer dest-dir)
                          (dtrace-info #:topic renderer "(~a ~a)" (object-name preamble-filter) src.tex)
                          
                          (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler renderer e (λ [] (custodian-shutdown-all (current-custodian)))))])
                            (define /dev/texin : Input-Port
                              (cond [(equal? src.tex TEXNAME.tex) (open-input-bytes (file->bytes src.tex))]
                                    [else (open-input-file src.tex)]))
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
                          
                          (custodian-shutdown-all (current-custodian)))
                        (tex-exec renderer latex TEXNAME.tex dest-dir retry))
                      (tex-exec renderer latex src.tex dest-dir retry)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-document-destination : (->* (Path-String) (Boolean #:extension Bytes) (Option Path))
  (lambda [tex [contained-in-package? #true] #:extension [ext #".pdf"]]
    (define dirname : (Option Path) (path-only tex))
    (define basename : (Option Path) (file-name-from-path tex))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     "typesetting" (path-replace-extension basename ext)))))

(define tex-document-extension : (-> Symbol [#:fallback (U Symbol Bytes)] Bytes)
  (lambda[renderer #:fallback [fallback #".pdf"]]
    (define latex : (Option Tex-Renderer)
      (hash-ref latex-database renderer
                (λ [] (and (symbol? fallback)
                           (hash-ref latex-database fallback (λ [] #false))))))
    (cond [(and latex) (tex-renderer-extension latex)]
          [(bytes? fallback) fallback]
          [else #".pdf"])))

(define tex-list-renderers : (-> (Listof Symbol))
  (lambda []
    (hash-keys latex-database)))
