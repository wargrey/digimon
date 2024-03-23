#lang typed/racket/gui

(provide main)

(require "nanomon/parameter.rkt")
(require "nanomon/shell.rkt")

(require "../continuation.rkt")
(require "../dtrace.rkt")
(require "../cmdopt.rkt")
(require "../debug.rkt")
(require "../thread.rkt")

(require "../digitama/minimal/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option nanomon-flags #: Nanomon-Flags
  #:program the-name
  #:args [shell file . args]

  #:usage-help "A utility to assist the building system"
  #:once-each
  [[(#\l lang)                                       lang          "replace the #lang line with ~1 for lexer"]
   [(#\w print-columns) #:=> cmdopt-string+>index columns #: Index ["use ~1 as the default width for pretty printing (default: ~a)"
                                                                    the-print-width]]
   [(#\s slient quiet)  #:=> nanomon-silent                        "suppress lang's standard output"]
   [(#\v verbose)       #:=> nanomon-verbose                       "run with verbose messages"]])

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
    (define root-thread : Thread (current-thread))
    (define root-custodian : Custodian (current-custodian))
    (parameterize ([current-nanomon-shell (nanomon-shell-name shell)]
                   [current-custodian (make-custodian)]
                   [current-error-port (open-output-dtrace 'error)]
                   [current-output-port (if (nanomon-silent) /dev/null (current-output-port))])
      (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)])
                (define ghostcat : Thread
                  (thread (λ [] (with-handlers ([exn:fail? (λ [[e : exn]] (thread-send root-thread e))]
                                                [exn:break? void])
                                  ((nanomon-shell-exec shell) target root-thread)))))

                (yield (thread-receive-evt))
                (let ([retcode (thread-receive)])
                  (cond [(byte? retcode) retcode]
                        [(not (exn:fail? retcode)) 0]
                        [else (dtrace-exception retcode #:level 'fatal #:brief? #false)
                              (nanomon-errno)])))
              (thread-safe-shutdown (current-custodian) root-custodian)))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (nanomon-restore-options!)
    (define-values (options λargv) (parse-nanomon-flags argument-list))

    (when (nanomon-flags-help? options)
      (wisemon-display-help))

    (define-values (name target argv) (λargv))
    
    (let ([tracer (thread (make-nanomon-log-trace))])
      (dtrace-info #:topic the-name "shell: ~a" name)
      
      (parameterize ([current-logger /dev/dtrace]
                     [nanomon-lang (nanomon-flags-lang options)]
                     [pretty-print-columns (or (nanomon-flags-print-columns options) the-print-width)]
                     [current-command-line-arguments (list->vector argv)])  
        (define shell : (Option Nanomon-Shell) (nanomon-shell-ref (string->symbol name)))
        
        (if (not shell)
            (let ([retcode (nanomon-errno)])
              (call-with-dtrace (λ [] (dtrace-fatal "fatal: unrecognized command")))
              (exit retcode))
            (exit (time* (begin0 (exec-shell shell (cmdopt-string->path the-name target))
                                 (dtrace-sentry-notice #:end? #true eof)
                                 (thread-wait tracer)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nanomon-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (dtrace-event-echo level message urgent topic)

    (when (and (exn:fail? urgent) (nanomon-verbose))
      (let ([/dev/stderr (open-output-string)])
        (display-continuation-stacks urgent /dev/stderr)
        (let ([errmsg (get-output-string /dev/stderr)])
          (unless (string=? errmsg "")
            (dtrace-event-echo 'trace (get-output-string /dev/stderr) #false topic)))))))

(define make-nanomon-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop #:default-receiver nanomon-event-echo
                      (cond [(nanomon-verbose) 'trace]
                            [else 'info]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
