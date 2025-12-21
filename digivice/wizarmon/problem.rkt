#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/digitama/spec/expect/exec))
(provide (all-from-out digimon/digitama/toolchain/spec/problem))

(require digimon/digitama/toolchain/spec/problem)
(require digimon/digitama/spec/expect/exec)
(require digimon/digitama/minimal/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wizarmon-spec-timeout : (Parameterof Natural) (make-parameter 0))
(define wizarmon-spec-strict : (Parameterof Boolean) (make-parameter #false))
(define wizarmon-spec-echo-lines : (Parameterof Natural) (make-parameter 32))
(define wizarmon-spec-error-limit : (Parameterof Natural) (make-parameter 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-spec-problem-config : (-> (Option Dtrace-Level) (-> Problem-Spec Spec-Exec.Cfg))
  (lambda [stdin-log-level]
    (λ [self]
      (make-spec-exec.cfg #:stdout (current-output-port)
                          #:stdin-log-level stdin-log-level
                          #:strict? (or (problem-spec-strict? self) (wizarmon-spec-strict))
                          #:echo-limit (or (problem-spec-stdio-lines self) (wizarmon-spec-echo-lines))
                          #:error-limit (wizarmon-spec-error-limit)
                          #:extras (list (cons 'timeout (or (problem-spec-timeout self)
                                                            (wizarmon-spec-timeout))))))))
