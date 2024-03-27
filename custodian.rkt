#lang typed/racket/base

(provide (all-defined-out))

(require "thread.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define call-in-nested-custodian : (All (a) (-> (-> a) a))
  (lambda [action]
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       void action
       (λ [] (custodian-shutdown-all (current-custodian)))))))

(define call-in-safe-custodian : (All (a) (-> (-> a) a))
  (lambda [action]
    (define root-custodian : Custodian (current-custodian))
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       void action
       (λ [] (thread-safe-shutdown (current-custodian) root-custodian))))))
