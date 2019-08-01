#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [call-with-continuation-prompt (All (a b) (-> (-> a) (Prompt-Tagof Any Any) (-> (-> Spec-Issue) b) (U a b)))]
 [abort-current-continuation (All (a) (-> (Prompt-Tagof Any Any) a Nothing))]
 [default-continuation-prompt-tag (-> (Prompt-Tagof Any Any))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: (Prompt-Tagof Any (-> a ... a b)) cannot be type-checked meanwhile since it involves 'chaperone/sc'

(define default-spec-prompt : (Parameterof (Prompt-Tagof Any Any)) (make-parameter (default-continuation-prompt-tag)))
(define default-spec-handler : (Parameterof (-> Spec-Issue Void)) (make-parameter default-spec-issue-display))

(define spec-story : (All (a b) (-> (Option Symbol) (-> a) (-> Spec-Issue b) (U a b)))
  (lambda [tagname do-task handle]
    (define current-prompt : (Prompt-Tagof Any Any)
      (cond [(not tagname) (default-continuation-prompt-tag)]
            [else (make-continuation-prompt-tag tagname)]))

    (parameterize ([default-spec-prompt current-prompt])
      (call-with-continuation-prompt do-task current-prompt
        (λ [[at-collapse : (-> Spec-Issue)]] : b
          (handle (at-collapse)))))))

(define spec-misbehave : (->* () ((U Spec-Issue Spec-Issue-Type exn:fail)) Nothing)
  (lambda [[v 'misbehaved]]
    (define handle : (-> Spec-Issue Void) (default-spec-handler))
    (define issue : Spec-Issue
      (cond [(symbol? v) (make-spec-issue v)]
            [(exn? v) (make-spec-panic-issue v)]
            [else v]))
    (abort-current-continuation (default-spec-prompt)
                                (λ [] (handle issue) issue))))
