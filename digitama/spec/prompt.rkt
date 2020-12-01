#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")
(require "../../continuation.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-issue-handler : (Parameterof (-> Spec-Issue Void)) (make-parameter default-spec-issue-display))

(define-values (spec-story abort-story)
  ((inst make-continuation-prompt-control Spec-Issue Spec-Issue Spec-Issue)
   'spec))

(define spec-misbehave : (->* () ((U Spec-Issue Spec-Issue-Type exn:fail)) Nothing)
  (lambda [[v 'misbehaved]]
    (define handle : (-> Spec-Issue Void) (default-spec-issue-handler))
    (define issue : Spec-Issue
      (cond [(symbol? v) (make-spec-issue v)]
            [(exn? v) (make-spec-panic-issue v)]
            [else v]))
    
    (abort-story (λ [] (handle issue) issue))))
