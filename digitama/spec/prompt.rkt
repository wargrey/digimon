#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")
(require "../../continuation.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-issue-handler : (Parameterof (-> Spec-Issue Void)) (make-parameter default-spec-issue-display))

(define-continuation-prompt-control (spec-story spec-misbehave) spec #:-> [Spec-Issue]
  #:default-abort-callback (λ [[issue : Spec-Issue]] ((default-spec-issue-handler) issue))
  #:default-handler-map values
  #:with (U Spec-Issue Spec-Issue-Type exn:fail) #:default 'misbehaved
  (λ [v] (cond [(symbol? v) (make-spec-issue v)]
               [(exn? v) (make-spec-panic-issue v)]
               [else v])))
