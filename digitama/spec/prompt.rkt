#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Prompt-Handler (-> (-> Spec-Issue) Spec-Issue))
(define-type Spec-Prompt-Tag (Prompt-Tagof Spec-Issue Spec-Prompt-Handler))

(define default-spec-prompt : (Parameterof Spec-Prompt-Tag) (make-parameter ((inst make-continuation-prompt-tag Spec-Issue Spec-Prompt-Handler) 'spec)))
(define default-spec-issue-handler : (Parameterof (-> Spec-Issue Void)) (make-parameter default-spec-issue-display))

(define spec-story : (-> (Option Symbol) (-> Spec-Issue) (-> Spec-Issue Spec-Issue) Spec-Issue)
  (lambda [tagname do-task handle]
    (define current-prompt : Spec-Prompt-Tag (make-continuation-prompt-tag (or tagname 'spec)))

    (parameterize ([default-spec-prompt current-prompt])
      (call-with-continuation-prompt do-task current-prompt
        (λ [[at-collapse : (-> Spec-Issue)]] : Spec-Issue
          (handle (at-collapse)))))))

(define spec-misbehave : (->* () ((U Spec-Issue Spec-Issue-Type exn:fail)) Nothing)
  (lambda [[v 'misbehaved]]
    (define handle : (-> Spec-Issue Void) (default-spec-issue-handler))
    (define issue : Spec-Issue
      (cond [(symbol? v) (make-spec-issue v)]
            [(exn? v) (make-spec-panic-issue v)]
            [else v]))
    
    (abort-current-continuation (default-spec-prompt)
                                (λ _ (handle issue) issue))))
