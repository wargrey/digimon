#lang typed/racket/base

(provide (all-defined-out))

(require "issue.rkt")
(require "expectation.rkt")
(require "prompt.rkt")

(struct spec-behavior
  ([brief : String]
   [action : (-> Void)])
  #:type-name Spec-Behavior
  #:transparent)

(struct spec-feature
  ([brief : String]
   [before : (-> Any)]
   [behaviors : (Listof (U Spec-Behavior Spec-Feature))]
   [after : (-> Any)])
  #:type-name Spec-Feature
  #:transparent)

(define make-spec-behavior : (-> (U String Symbol) (-> Void) [#:before (-> Any)] [#:after (-> Any)] Spec-Behavior)
  (lambda [name action #:before [setup void] #:after [teardown void]]
    (spec-behavior (if (symbol? name) (symbol->string name) name)
                   (cond [(and (eq? setup void) (eq? teardown void)) action]
                         [else (λ [] (dynamic-wind setup action teardown))]))))

(define make-spec-feature : (-> (U String Symbol) (Listof (U Spec-Behavior Spec-Feature)) [#:before (-> Any)] [#:after (-> Any)] Spec-Feature)
  (lambda [name behaviors #:type [type '||] #:before [setup void] #:after [teardown void]]
    (spec-feature (if (symbol? name) (symbol->string name) name)
                  setup behaviors teardown)))
