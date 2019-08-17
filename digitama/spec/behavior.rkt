#lang typed/racket/base

(provide (all-defined-out))

(require/typed racket
               [object-name (-> Procedure Symbol)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Spec-Feature-Upfold s) (-> String (-> Any) (-> Any) s s s))
(define-type (Spec-Feature-Downfold s) (-> String (-> Any) (-> Any) s (Option s)))
(define-type (Spec-Feature-Herefold s) (-> String (-> Void) s s))

(struct spec-behavior
  ([brief : String]
   [evaluation : (-> Void)])
  #:type-name Spec-Behavior
  #:transparent)

(struct spec-feature
  ([brief : String]
   [evaluation : (All (s) (-> (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s s))]
   [before : (-> Any)]
   [after : (-> Any)])
  #:type-name Spec-Feature
  #:transparent)

(define make-spec-behavior : (-> (U String Symbol Procedure) (-> Void) [#:before (-> Any)] [#:after (-> Any)] Spec-Behavior)
  (lambda [name evaluation #:before [setup void] #:after [teardown void]]
    (spec-behavior (spec-name->brief name)
                   (cond [(and (eq? setup void) (eq? teardown void)) evaluation]
                         [else (λ [] (dynamic-wind setup evaluation teardown))]))))

(define make-spec-feature : (-> (U String Symbol Procedure) (Listof (U Spec-Feature Spec-Behavior)) [#:before (-> Any)] [#:after (-> Any)] Spec-Feature)
  (lambda [name behaviors #:before [setup void] #:after [teardown void]]
    (define #:forall (s) (evaluation [downfold : (Spec-Feature-Downfold s)] [upfold : (Spec-Feature-Upfold s)] [herefold : (Spec-Feature-Herefold s)] [seed : s]) : s
      (for/fold ([seed : s seed])
                ([b (in-list behaviors)])
        (if (spec-behavior? b)
            (herefold (spec-behavior-brief b) (spec-behavior-evaluation b) seed)
            (spec-feature-fold-pace b downfold upfold herefold seed))))
    (spec-feature (spec-name->brief name)
                  evaluation setup teardown)))

(define spec-behaviors-fold : (All (s) (-> (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s (U Spec-Feature Spec-Behavior) s))
  (lambda [downfold upfold herefold seed feature]
    (if (spec-feature? feature)
        (spec-feature-fold-pace feature downfold upfold herefold seed)
        (herefold (spec-behavior-brief feature) (spec-behavior-evaluation feature) seed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-feature-fold-pace : (All (s) (-> Spec-Feature (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s s))
  (lambda [feature downfold upfold herefold seed]
    (let* ([brief (spec-feature-brief feature)]
           [before (spec-feature-before feature)]
           [after (spec-feature-after feature)]
           [maybe-kid-seed (downfold brief before after seed)])
      (cond [(not maybe-kid-seed) seed]
            [else (upfold brief before after seed
                          ((spec-feature-evaluation feature)
                           downfold upfold herefold maybe-kid-seed))]))))

(define spec-name->brief : (-> (U String Symbol Procedure) String)
  (lambda [name]
    (cond [(procedure? name) (symbol->string (object-name name))]
          [(symbol? name) (symbol->string name)]
          [else name])))