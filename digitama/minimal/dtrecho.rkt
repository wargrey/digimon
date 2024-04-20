#lang typed/racket/base

(provide (all-defined-out))

(require "dtrace.rkt")

(require "../dtrace.rkt")
(require "../../echo.rkt")
(require "../../continuation.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-event-echo : Dtrace-Receiver
  (lambda [level message urgent topic]
    (case level
      [(note) (displayln message)]
      [(info) (echof #:fgcolor 'cyan "~a~n" message)]
      [(notice) (echof #:fgcolor 'green "~a~n" message)]
      [(warning) (echof #:fgcolor 'yellow "~a~n" message)]
      [(error) (echof #:fgcolor 'red "~a~n" message)]
      [(fatal) (echof #:fgcolor 'darkred "~a~n" message)]
      [(critical alert emergency) (echof #:fgcolor 'darkred #:attributes '(inverse) "~a~n" message)]
      [(debug) (echof #:fgcolor 'darkgray "~a~n" message)]
      [else (echof #:fgcolor 'darkgray #:attributes '(inverse) "~a~n" message)])))

(define dtrace-sync : (-> Void)
  (lambda []
    (define ghostcat (thread thread-receive))
    (dtrace-sentry-info "use this if you don't want dtrace output is interleaved with standard output"
                        #:handler (λ whocares (thread-send ghostcat 'okay)) #:end? #false)
    (thread-wait ghostcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtrace-exception : (->* (exn) (#:topic Any #:level Symbol #:prefix? Boolean #:brief? Boolean) Void)
  (lambda [errobj #:topic [topic /dev/dtrace] #:level [level 'error] #:prefix? [prefix? #true] #:brief? [brief? #true]]
    (define /dev/errout : Output-Port (open-output-string))

    (display (object-name errobj) /dev/errout)
    (display #\: /dev/errout)
    (display #\space /dev/errout)
    (display (exn-message errobj) /dev/errout)

    (when (not brief?)
      (let ([stacks (continuation-mark->stacks errobj)])
        (when (pair? stacks)
          (display #\newline /dev/errout)
          (display-continuation-stacks stacks /dev/errout))))
    
    (dtrace-send topic level (get-output-string /dev/errout) errobj prefix?)))
