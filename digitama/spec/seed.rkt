#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")

(define-type Spec-Summary (Immutable-HashTable Spec-Issue-Type Natural))

(struct (Datum) spec-seed
  ([datum : Datum]
   [namepath : (Listof String)]
   [summary : Spec-Summary]
   [exceptions : (Listof (Option exn:fail))])
  #:type-name Spec-Seed)

(define make-spec-seed : (All (s) (-> s (Spec-Seed s)))
  (lambda [datum]
    ((inst spec-seed s) datum null (make-immutable-hasheq) null)))

(define spec-seed-copy : (All (s) (-> (Spec-Seed s) s (Listof String)
                                      [#:summary (Option Spec-Summary)] [#:exceptions (Option (Listof (Option exn:fail)))]
                                      (Spec-Seed s)))
  (lambda [seed datum namepath #:summary [summary #false] #:exceptions [exceptions #false]]
    (spec-seed datum namepath
               (or summary (spec-seed-summary seed))
               (or exceptions (spec-seed-exceptions seed)))))
