#lang typed/racket/base

(provide (all-defined-out))

(require "../../symbol.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Spec-Feature-Upfold s) (-> String (-> Any) (-> Any) s s s))
(define-type (Spec-Feature-Downfold s) (-> String (-> Any) (-> Any) s (Option s)))
(define-type (Spec-Feature-Herefold s) (-> String (-> Void) Natural s s))
(define-type Described-Behaviors (U Spec-Feature Spec-Behavior (Listof (U Spec-Feature Spec-Behavior))))

(struct spec-behavior
  ([brief : String]
   [evaluate : (-> Void)]
   [timeout/ms : Natural])
  #:type-name Spec-Behavior
  #:transparent)

(struct spec-feature
  ([brief : String]
   [evaluate : (All (s) (-> (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s s))]
   [before : (-> Any)]
   [after : (-> Any)]
   [parameterize : (Option (-> Void))])
  #:type-name Spec-Feature
  #:transparent)

(define make-spec-behavior : (-> (U String Symbol Procedure) (-> Void)
                                 [#:before (-> Any)] [#:after (-> Any)] [#:parameterize (Option (-> Void))] [#:timeout Natural]
                                 Spec-Behavior)
  (lambda [name evaluation #:before [setup void] #:after [teardown void] #:parameterize [parameterize #false] #:timeout [ms 0]]
    (define evaluate : (-> Void)
      (cond [(and (eq? setup void) (eq? teardown void)) evaluation]
            [else (λ [] (dynamic-wind setup evaluation teardown))]))
    
    (spec-behavior (spec-name->brief name)
                   (cond [(not parameterize) evaluate]
                         [else (λ [] (call-in-nested-thread
                                      (λ [] (parameterize) (evaluate))))])
                   ms)))

(define make-spec-feature : (-> (U String Symbol Procedure) (Listof (-> Described-Behaviors))
                                [#:before (-> Any)] [#:after (-> Any)] [#:parameterize (Option (-> Void))] Spec-Feature)
  (lambda [name make-behaviors #:before [setup void] #:after [teardown void] #:parameterize [parameterize #false]]
    (define #:forall (s) (evaluate [downfold : (Spec-Feature-Downfold s)] [upfold : (Spec-Feature-Upfold s)] [herefold : (Spec-Feature-Herefold s)] [seed : s]) : s
      ; please keep the order of evaluation exactly as that defining the features and behaviors
      ;   since the evaluation implicitly occurs inside its closure, which might depend on some
      ;   pre-conditions and even other features and behaviors
      (let flatten-eval ([seed : s seed]
                         [behavior : (Listof (U (-> Described-Behaviors) Described-Behaviors)) make-behaviors])
        (for/fold ([seed : s seed])
                  ([behavior (in-list behavior)])
          (let eval-behavior ([behavior : (U (-> Described-Behaviors) Described-Behaviors) behavior])
            (cond [(spec-behavior? behavior) (herefold (spec-behavior-brief behavior) (spec-behavior-evaluate behavior) (spec-behavior-timeout/ms behavior) seed)]
                  [(spec-feature? behavior) (spec-feature-fold-pace behavior downfold upfold herefold seed)]
                  [(list? behavior) (flatten-eval seed behavior)]
                  [else (eval-behavior (behavior))])))))
    
    (spec-feature (spec-name->brief name) evaluate setup teardown parameterize)))

(define spec-behaviors-fold : (All (s) (-> (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s (U Spec-Feature Spec-Behavior) s))
  (lambda [downfold upfold herefold seed feature]
    (if (spec-feature? feature)
        (spec-feature-fold-pace feature downfold upfold herefold seed)
        (herefold (spec-behavior-brief feature) (spec-behavior-evaluate feature) (spec-behavior-timeout/ms feature) seed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-feature-fold-pace : (All (s) (-> Spec-Feature (Spec-Feature-Downfold s) (Spec-Feature-Upfold s) (Spec-Feature-Herefold s) s s))
  (lambda [feature downfold upfold herefold seed]
    (let* ([brief (spec-feature-brief feature)]
           [before (spec-feature-before feature)]
           [after (spec-feature-after feature)]
           [parameterize (spec-feature-parameterize feature)]
           [maybe-kid-seed (downfold brief before after seed)])
      (define (evaluate) : s
        (cond [(not maybe-kid-seed) seed]
              [else (upfold brief before after seed
                            ((spec-feature-evaluate feature)
                             downfold upfold herefold maybe-kid-seed))]))
      (cond [(not parameterize) (evaluate)]
            [else (call-in-nested-thread
                      (λ [] (parameterize) (evaluate)))]))))

(define spec-name->brief : (-> (U String Symbol Procedure) String)
  (lambda [name]
    (cond [(procedure? name) (symbol->immutable-string (datum-name name))]
          [(symbol? name) (symbol->immutable-string name)]
          [else name])))
