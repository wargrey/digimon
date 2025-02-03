#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../exec.rkt"))

(require "engine.rkt")

(require "../exec.rkt")
(require "../minimal/dtrace.rkt")
(require "../../filesystem.rkt")
(require "../../debug.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-exec : (-> Symbol Tex-Engine Path-String Path-String Byte Boolean Boolean Path)
  (lambda [renderer latex TEXNAME.tex dest-dir retry halt-on-error? shell?]
    (define post-exec : (Option Tex-Post-Exec) (tex-engine-post-exec latex))
    (define TEXNAME : Path (assert (file-name-from-path TEXNAME.tex) path?))
    (define TEXNAME.ext : Path (build-path dest-dir (path-replace-extension TEXNAME (tex-engine-extension latex))))
    (define TEXNAME.log : Path (build-path dest-dir (path-replace-extension TEXNAME #".log")))
    (define log-timestamp : Natural (file-mtime TEXNAME.log))
    (define options : (Listof (Listof String))
      (list (list "-interaction=batchmode")
            (if (not halt-on-error?) null (list "-halt-on-error"))
            (list (if (not shell?) "-no-shell-escape" "-shell-escape"))
            (list (format "-output-directory=~a" dest-dir))
            (list (cond [(string? TEXNAME.tex) TEXNAME.tex]
                        [else (path->string TEXNAME.tex)]))))
    
    (unless (directory-exists? dest-dir)
      (fg-recon-mkdir renderer dest-dir))

    (unless (unbox &tex-env)
      (define tex-env (environment-variables-copy (current-environment-variables)))

      ; to tell tex engines don't break console/log lines too early
      ; also tex doesn't like too small `max_print_line` and will refuse to execute if so
      (environment-variables-set! tex-env #"max_print_line" #"1024")
      (set-box! &tex-env tex-env))

    ;;; NOTE: Scribble may generate resources in `(current-directory)`
    (parameterize ([current-directory dest-dir])
      (let rerun ([times : Natural 1])
        (time* (fg-recon-exec #:env (unbox &tex-env)
                              (string->symbol (format "~a[~a]" renderer times))
                              (tex-engine-program latex)
                              (if (= times 1) (tex-draftmode latex options) options)
                              (λ [[op : Symbol] [program : Path] [status : Nonnegative-Integer] [errmsg : Bytes]]
                                (define log-now (file-mtime TEXNAME.log))
                                (cond [(<= log-now log-timestamp) (dtrace-warning #:topic op #:prefix? #true "log has not updated")]
                                      [(> log-now 0) (tex-log-cat renderer TEXNAME.log)]))))
        
        (when (and (file-exists? TEXNAME.log) (> (file-mtime TEXNAME.log) log-timestamp))
          ;;; NOTE
          ;; see if we get a "Rerun" note, these seem to come in two flavors
          ;; * Label(s) may have changed. Rerun to get cross-references right.
          ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
          (when (call-with-input-file* TEXNAME.log
                  (lambda [[/dev/login : Input-Port]]
                    (regexp-match? #px#"changed\\.\\s+Rerun" /dev/login)))
            (cond [(>= times retry) (raise-user-error renderer "could not get a stable result after ~a runs" times)]
                  [else (rerun (+ times 1))])))))
    
    (cond [(not post-exec) TEXNAME.ext]
          [else (post-exec renderer TEXNAME.ext)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &tex-env : (Boxof (Option Environment-Variables)) (box #false))

(define tex-log-cat : (-> Symbol Path Void)
  (lambda [renderer TEXNAME.log]
    (call-with-input-file* TEXNAME.log
      (λ [[/dev/login : Input-Port]]
        (let cat : Void ([error? : Boolean #false])
          (define log (read-line /dev/login 'any))
          (when (string? log)
            (cond [(string=? log "") (cat error?)]
                  [(eq? (string-ref log 0) #\\) (cat error?)] ; skip tex code line
                  [(eq? (string-ref log 0) #\!) (dtrace-error log #:topic renderer) (cat #true)]
                  [(not error?) (dtrace-debug log #:topic renderer) (cat error?)]
                  [(regexp-match? #px"^l[.]\\d+" log) (dtrace-note log #:topic renderer) (cat #false)]
                  [else (dtrace-debug log #:topic renderer) (cat error?)])))))))

(define tex-draftmode : (-> Tex-Engine (Listof (Listof String)) (Listof (Listof String)))
  (lambda [engine options]
    (define maybe-draft (tex-engine-draftmode-option engine))

    (cond [(not maybe-draft) options]
          [else (cons (list maybe-draft)
                      options)])))
