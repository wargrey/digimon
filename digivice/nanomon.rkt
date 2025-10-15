#lang typed/racket/gui

(provide main)

(require "nanomon/parameter.rkt")
(require "nanomon/shell.rkt")
(require "wizarmon/echo.rkt")

(require "../dtrace.rkt")
(require "../cmdopt.rkt")
(require "../debug.rkt")
(require "../custodian.rkt")

(require "../digitama/minimal/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option nanomon-flags #: Nanomon-Flags
  #:program the-name
  #:args [shell . args]

  #:usage-help "A utility to assist the building system"
  #:once-each
  [[(#\w print-columns) #:=> cmdopt-string+>index columns #: Index ["use ~1 as the default width for pretty printing (default: ~a)"
                                                                    the-print-width]]
   [(#\s slient quiet)  #:=> nanomon-silent                        "suppress lang's standard output"]
   [(#\d debug)         #:=> nanomon-debug                         "run with debug information"]
   [(#\v verbose)       #:=> nanomon-verbose                       "run with verbose messages"]])

(define wisemon-display-help : (->* () ((Option Byte)) Void)
  (lambda [[retcode 0]]
    (define shells : (Immutable-HashTable Symbol Nanomon-Shell) (nanomon-list-shells))
    (define shell-helps : (Listof String)
      (for/list ([(n s) (in-hash shells)])
        (format "    ~a : ~a" n (nanomon-shell-description s))))

    (define foreign-helps : (Listof String)
      (for/list : (Listof String) ([sh (in-hash-values (nanomon-list-foreign-shells))])
        (format "    ~a : ~a" (nanomon-shell-name sh) (nanomon-shell-description sh))))
    
    (display-nanomon-flags #:more-ps (cons "  where <shell> is one of"
                                           (cond [(null? foreign-helps) shell-helps]
                                                 [else (append shell-helps (list " ") foreign-helps)]))
                           #:exit retcode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : (-> Nanomon-Shell (Listof String) Byte)
  (lambda [shell argv]
    (call-in-nested-custodian
     (λ [] (let ([root-thread : Thread (current-thread)])
             (parameterize ([current-nanomon-shell (nanomon-shell-name shell)]
                            [current-error-port (open-output-dtrace 'error)]
                            [current-output-port (if (nanomon-silent) /dev/null (current-output-port))])
               (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)])
                 (define ghostcat : Thread
                   (thread (λ [] (with-handlers ([exn:fail? (λ [[e : exn]] (thread-send root-thread e))]
                                                 [exn:break? void])
                                   ((nanomon-shell-exec shell) argv root-thread)))))
                 
                 (yield (thread-receive-evt))
                 (let ([retcode (thread-receive)])
                   (cond [(byte? retcode) retcode]
                         [(not (exn:fail? retcode)) 0]
                         [else (dtrace-exception retcode #:level 'fatal #:brief? #false)
                               (nanomon-errno)])))))))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (nanomon-restore-options!)
    (define-values (options λargv) (parse-nanomon-flags argument-list))

    (when (nanomon-flags-help? options)
      (wisemon-display-help))

    (define-values (name argv) (λargv))
    
    (let ([tracer (thread (make-wizarmon-log-trace (nanomon-debug) (nanomon-verbose)))])
      (dtrace-info #:topic the-name "shell: ~a" name)
      
      (parameterize ([current-logger /dev/dtrace]
                     [pretty-print-columns (or (nanomon-flags-print-columns options) the-print-width)])  
        (define shell : (Option Nanomon-Shell) (nanomon-shell-ref (string->symbol name)))
        
        (if (not shell)
            (let ([retcode (nanomon-errno)])
              (call-with-dtrace (λ [] (dtrace-fatal "fatal: unrecognized command")))
              (exit retcode))
            (exit (time* (begin0 (exec-shell shell argv)
                                 (dtrace-sentry-notice #:end? #true eof)
                                 (thread-wait tracer)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
