#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "problem.rkt")
(require "exec/c.rkt")
(require "exec/lean.rkt")
(require "exec/python.rkt")

(require "../../cmdopt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option spec-flags #: Spec-Flags
  #:program '|wizarmon spec|
  #:args args

  #:usage-help "set spec configuration"
  #:ps "options after `--` would be passed to the subprocess"
  #:once-each
  [[(timeout)           #:=> cmdopt-string->natural ms    #: Natural  "set the timeout of execution to ~1 millisecond"]
   [(#\n lines)         #:=> cmdopt-string->natural count #: Natural  ["set the echo lines of the stdio to ~1 (default: ~a)"
                                                                       (wizarmon-spec-echo-lines)]]
   [(error-limit)       #:=> cmdopt-string->natural count #: Natural  ["set the error limit to ~1 (default: ~a)"
                                                                       (wizarmon-spec-error-limit)]]
   [(#\S strict)        #:=> wizarmon-spec-strict                     "check expected output of a program strictly"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-spec/flags : (-> Path Symbol Any)
  (lambda [path lang-name]
    (define-values (options λargv) (parse-spec-flags))

    (cond [(spec-flags-help? options) (display-spec-flags)]
          [else (parameterize ([wizarmon-spec-timeout (or (spec-flags-timeout options) (wizarmon-spec-timeout))]
                               [wizarmon-spec-strict (spec-flags-strict options)]
                               [wizarmon-spec-echo-lines (or (spec-flags-lines options) (wizarmon-spec-echo-lines))]
                               [current-command-line-arguments (list->vector (λargv))])
                  (shell-spec path lang-name))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-spec : (-> Path Symbol Any)
  (lambda [path lang-name]
    (case/eq lang-name
      [(cpp) (shell-cpp path 'cpp)]
      [(c) (shell-c path 'c)]
      [(lean) (shell-lean path 'lean)]
      [(python) (shell-python path 'python)])))
