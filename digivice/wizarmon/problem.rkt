#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../../digitama/spec/expect/exec.rkt"))
(provide (all-from-out "../../digitama/toolchain/spec/problem.rkt"))

(require "../../digitama/toolchain/spec/problem.rkt")
(require "../../digitama/spec/expect/exec.rkt")

(require "../../digitama/minimal/dtrace.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Problem-Config (-> Problem-Spec Spec-Exec.Cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-spec-timeout : (Parameterof Natural) (make-parameter 0))
(define wizarmon-spec-strict : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-spec-echo-lines : (Parameterof Natural) (make-parameter 32))
(define wizarmon-spec-error-limit : (Parameterof Natural) (make-parameter 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-spec-problem-config : (-> (Option Dtrace-Level) Problem-Config)
  (lambda [stdin-log-level]
    (λ [self]
      (make-spec-exec.cfg #:stdout (current-output-port)
                          #:stdin-log-level stdin-log-level
                          #:strict? (or (problem-spec-strict? self) (wizarmon-spec-strict))
                          #:echo-limit (or (problem-spec-stdio-lines self) (wizarmon-spec-echo-lines))
                          #:error-limit (wizarmon-spec-error-limit)
                          #:extras (list (cons 'timeout (or (problem-spec-timeout self)
                                                            (wizarmon-spec-timeout))))))))
