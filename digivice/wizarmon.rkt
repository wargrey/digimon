#lang typed/racket/gui

(provide main)

(require "wizarmon/parameter.rkt")
(require "wizarmon/exec.rkt")

(require "../continuation.rkt")
(require "../dtrace.rkt")
(require "../cmdopt.rkt")
(require "../debug.rkt")
(require "../thread.rkt")

(require "../digitama/minimal/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option wizarmon-flags #: Wizarmon-Flags
  #:program the-name
  #:args [file . args]

  #:usage-help "A utility to assist the building system"
  #:once-each
  [[(#\l lang)                                       lang          "treat the source as having the type ~1"]
   [(#\w print-columns) #:=> cmdopt-string+>index columns #: Index ["use ~1 as the default width for pretty printing (default: ~a)"
                                                                    the-print-width]]
   [(#\s slient quiet)  #:=> wizarmon-silent                        "suppress lang's standard output"]
   [(#\v verbose)       #:=> wizarmon-verbose                       "run with verbose messages"]

   ; The C++ extension for VSCode isn't smart enough
   ;   we have to trick it for not launching the debugger when
   ;   our task script has already run the program.
   ; Besides, in Windows, the debugger always starts another terminal
   ;   and causes the one running our task script hidden.
   [(pretend)           #:=> cmdopt-string->byte status #: Byte    "replace normal exit status with ~1"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exec-shell : (-> Path Byte)
  (lambda [target]
    (define root-thread : Thread (current-thread))
    (define root-custodian : Custodian (current-custodian))
    (parameterize ([current-custodian (make-custodian)]
                   [current-error-port (open-output-dtrace 'error)]
                   [current-output-port (if (wizarmon-silent) /dev/null (current-output-port))])
      (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)])
                (define ghostcat : Thread
                  (thread (λ [] (with-handlers ([exn:fail? (λ [[e : exn]] (thread-send root-thread e))]
                                                [exn:break? void])
                                  (shell~~exec target root-thread)))))

                (yield (thread-receive-evt))
                (let ([retcode (thread-receive)])
                  (cond [(byte? retcode) retcode]
                        [(not (exn:fail? retcode)) 0]
                        [else (dtrace-exception retcode #:level 'fatal #:brief? #false)
                              (wizarmon-errno)])))
              (thread-safe-shutdown (current-custodian) root-custodian)))))

(define trick-exit : (-> Byte (Option Byte) Nothing)
  (lambda [status pretend]
    (exit
     (cond [(not pretend) status]
           [(zero? status) pretend]
           [else status]))))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (wizarmon-restore-options!)
    (define-values (options λargv) (parse-wizarmon-flags argument-list))

    (when (wizarmon-flags-help? options)
      (display-wizarmon-flags #:exit 0))

    (define-values (target argv) (λargv))
    
    (let ([tracer (thread (make-wizarmon-log-trace))])
      (dtrace-notice #:topic the-name "source: ~a" target)
      
      (parameterize ([current-logger /dev/dtrace]
                     [wizarmon-lang (wizarmon-flags-lang options)]
                     [pretty-print-columns (or (wizarmon-flags-print-columns options) the-print-width)]
                     [current-command-line-arguments (list->vector argv)])  
        (define pretend-status : (Option Byte) (wizarmon-flags-pretend options))
        
        (trick-exit (time* (begin0 (exec-shell (cmdopt-string->path the-name target))
                                   (dtrace-sentry-notice #:end? #true eof)
                                   (thread-wait tracer)))
                    pretend-status)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (dtrace-event-echo level message urgent topic)

    (when (and (exn:fail? urgent) (wizarmon-verbose))
      (let ([/dev/stderr (open-output-string)])
        (display-continuation-stacks urgent /dev/stderr)
        (let ([errmsg (get-output-string /dev/stderr)])
          (unless (string=? errmsg "")
            (dtrace-event-echo 'trace (get-output-string /dev/stderr) #false topic)))))))

(define make-wizarmon-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop #:default-receiver wizarmon-event-echo
                      (cond [(wizarmon-verbose) 'trace]
                            [else 'info]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
