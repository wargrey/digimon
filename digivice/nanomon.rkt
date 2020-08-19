#lang typed/racket/base

(provide main)

(require racket/path)

(require "nanomon/parameter.rkt")
(require "nanomon/shell.rkt")

(require "../dtrace.rkt")
(require "../cmdopt.rkt")
(require "../debug.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option nanomon-flags #: Nanomon-Flags
  #:program the-name
  #:args [shell file]

  #:usage-help "A utility for testing #lang"
  #:once-each
  [[(#\l lang)                          lang  "replace the #lang line with ~1"]
   [(#\v verbose) #:=> nanomon-verbose        "run with verbose messages"]])

(define wisemon-display-help : (->* () ((Option Byte)) Void)
  (lambda [[retcode 0]]
    (define shells : (Immutable-HashTable Symbol Nanomon-Shell) (nanomon-list-shells))
    (define shell-helps : (Listof String)
      (for/list ([(n s) (in-hash shells)])
        (format "    ~a : ~a" n (nanomon-shell-description s))))
    
    (display-nanomon-flags #:more-ps (cons "  where <shell> is one of" shell-helps)
                           #:exit retcode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : (-> Nanomon-Shell Path Byte)
  (lambda [shell target]
    (parameterize ([current-nanomon-shell (nanomon-shell-name shell)]
                   [current-custodian (make-custodian)])
      (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)]
                              [exn:fail? (λ [[e : exn]] (dtrace-exception e #:level 'fatal #:brief? #false) (nanomon-errno))])
                ((nanomon-shell-exec shell) target)
                0)
              (custodian-shutdown-all (current-custodian))))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (nanomon-restore-options!)
    (define-values (options λargv) (parse-nanomon-flags argument-list))

    (when (nanomon-flags-help? options)
      (wisemon-display-help))

    (parameterize ([current-logger /dev/dtrace]
                   [nanomon-lang (nanomon-flags-lang options)])
      (define name+target : (List String String) (λargv))
      (define shell : (Option Nanomon-Shell) (nanomon-shell-ref (string->symbol (car name+target))))

      (if (not shell)
          (let ([retcode (nanomon-errno)])
            (call-with-dtrace (λ [] (dtrace-fatal "fatal: unrecognized command")))
            (exit retcode))
          (exit (time-apply* (λ [] (let ([tracer (thread (make-lang-log-trace))])
                                     (begin0 (exec-shell shell (cmdopt-string->path the-name (cadr name+target)))
                                             (dtrace-datum-notice eof)
                                             (thread-wait tracer))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-lang-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (cond [(nanomon-verbose) 'trace]
                            [else 'info]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
