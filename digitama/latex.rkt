#lang typed/racket/base

(provide (all-defined-out) tex-engine Tex-Engine)

(require racket/port)
(require racket/string)

(require "typeset/engine.rkt")
(require "typeset/exec.rkt")
(require "typeset/tex.rkt")
(require "exec.rkt")

(require "../filesystem.rkt")
(require "minimal/dtrace.rkt")

; register renders
(require (submod "typeset/bin/pdftex.rkt" register))
(require (submod "typeset/bin/xetex.rkt" register))
(require (submod "typeset/bin/euptex.rkt" register))
(require (submod "typeset/bin/luahbtex.rkt" register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-render : (->* (Symbol Path-String Path-String)
                          (#:dest-subdir (Option Path-String) #:dest-copy? Boolean
                           #:fallback Symbol #:retry Byte #:enable-filter Boolean
                           #:halt-on-error? Boolean #:shell-escape? Boolean)
                          Path)
  (lambda [#:dest-subdir [dest-subdir #false] #:dest-copy? [dest-copy? #false]
           #:fallback [fallback 'latex] #:retry [retry 4] #:enable-filter [enable-filter #false]
           #:halt-on-error? [on-error-stop? #true] #:shell-escape? [shell? #true]
           engine src.tex dest-dir0]
    (unless (file-exists? src.tex)
      (error (or engine fallback) "no such tex file: ~a" src.tex))

    (define latex : Tex-Engine
      (hash-ref latex-database engine
                (λ [] (hash-ref latex-database fallback
                                (λ [] (error (or engine fallback) "no such tex engine in ~a"
                                             (tex-list-engines)))))))

    (define preamble-filter : (Option Tex-Preamble-Filter) (tex-engine-preamble-filter latex))

    (define dest-dir : Path-String
      (cond [(not dest-subdir) dest-dir0]
            [(absolute-path? dest-subdir) dest-subdir]
            [else (build-path dest-dir0 dest-subdir)]))

    (define dest.ext : Path
      (if (and enable-filter preamble-filter)
          (let ([TEXNAME.tex (build-path dest-dir (assert (file-name-from-path src.tex) path?))])
            (parameterize ([current-custodian (make-custodian)])
              
              (unless (directory-exists? dest-dir)
                (fg-recon-mkdir engine dest-dir))
              
              (dtrace-info #:topic engine "(~a ~a)" (object-name preamble-filter) src.tex)
              
              (with-handlers ([exn? (λ [[e : exn]] (fg-recon-handler engine e (λ [] (custodian-shutdown-all (current-custodian)))))])
                (define /dev/texin : Input-Port
                  (cond [(equal? src.tex TEXNAME.tex) (open-input-bytes (file->bytes src.tex))]
                        [else (open-input-file src.tex)]))
                (define /dev/texout : Output-Port (open-output-file TEXNAME.tex #:exists 'replace))
                
                (let copy-filter ([fstatus : Any 'SOF])
                  (unless (regexp-match-peek #px"^\\\\begin[{]document[}]" /dev/texin)
                    (define src-line : (U String EOF) (read-line /dev/texin 'any))
                    (when (string? src-line)
                      (define-values (maybe-line cstatus) (preamble-filter src-line fstatus))
                      (cond [(string? maybe-line) (displayln maybe-line /dev/texout)]
                            [(pair? maybe-line) (displayln (string-join maybe-line "\n") /dev/texout)])
                      (unless (eq? cstatus 'EOF)
                        (copy-filter cstatus)))))
                (copy-port /dev/texin /dev/texout))
            
              (custodian-shutdown-all (current-custodian)))
            (tex-exec engine latex TEXNAME.tex dest-dir retry on-error-stop? shell?))
          (tex-exec engine latex src.tex dest-dir retry on-error-stop? shell?)))

    (let ([TEXNAME.ext (build-path dest-dir0 (assert (file-name-from-path dest.ext)))])
      (if (not dest-copy?)
          (fg-mv engine dest.ext TEXNAME.ext)
          (fg-cp engine dest.ext TEXNAME.ext))
      TEXNAME.ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-document-destination : (->* (Path-String) (Boolean #:extension Bytes #:dest-dirname (Option String)) (Option Path))
  (lambda [tex [contained-in-package? #true] #:extension [ext #".pdf"] #:dest-dirname [rootdir #false]]
    (define dirname : (Option Path) (path-only tex))
    (define basename : (Option Path) (file-name-from-path tex))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     (or rootdir "typesetting") (path-replace-extension basename ext)))))

(define tex-document-extension : (-> Symbol [#:fallback (U Symbol Bytes)] Bytes)
  (lambda[engine #:fallback [fallback #".pdf"]]
    (define latex : (Option Tex-Engine)
      (hash-ref latex-database engine
                (λ [] (and (symbol? fallback)
                           (hash-ref latex-database fallback (λ [] #false))))))
    (cond [(and latex) (tex-engine-extension latex)]
          [(bytes? fallback) fallback]
          [else #".pdf"])))

(define tex-list-engines : (-> (Listof Symbol))
  (lambda []
    (hash-keys latex-database)))
