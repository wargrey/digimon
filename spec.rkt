#lang typed/racket/base

(provide (all-defined-out))
(provide spec-feature? spec-behavior? spec-feature-brief spec-behavior-brief)
(provide make-spec-behavior make-spec-feature spec-behaviors-fold)
(provide define-feature describe Spec-Summary)

(provide (all-from-out "digitama/spec/issue.rkt"))
(provide (all-from-out "digitama/spec/expectation.rkt"))

(require "digitama/spec/issue.rkt")
(require "digitama/spec/prompt.rkt")
(require "digitama/spec/seed.rkt")

(require "digitama/spec/expectation.rkt")
(require "digitama/spec/behavior.rkt")
(require "digitama/spec/dsl.rkt")

(require "format.rkt")
(require "echo.rkt")

(require racket/string)

(require (for-syntax racket/base))

(define-syntax (spec-begin stx)
  (syntax-case stx [:]
    [(_ id expr ...)
     #'(void (spec-prove (describe 'id expr ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Prove-Behavior (-> String (Listof String) (-> Void) Spec-Issue))

(define spec-prove-behavior : Spec-Prove-Behavior
  (lambda [brief namepath evaluation]
    ((inst spec-story Spec-Issue Spec-Issue)
     (gensym brief)
     (λ [] (parameterize ([default-spec-issue-brief brief])
             (with-handlers ([exn:fail? spec-misbehave])
               (evaluation)
               (make-spec-issue 'pass))))
     values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-summary-fold : (All (s) (-> (U Spec-Feature Spec-Behavior) s
                                         #:downfold (-> String s s) #:upfold (-> String s s s) #:herefold (-> Spec-Issue s s) [#:prove Spec-Prove-Behavior]
                                         Spec-Summary))
  (lambda [feature seed:datum #:downfold downfold #:upfold upfold #:herefold herefold #:prove [prove spec-prove-behavior]]
    (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines from shutting the current custodian accidently.
      (define (downfold-feature [name : String] [pre-action : (-> Any)] [post-action : (-> Any)] [seed : (Spec-Seed s)]) : (Spec-Seed s)
        (define maybe-exn : (Option exn:fail) (with-handlers ([exn:fail? (λ [[e : exn:fail]] e)]) (pre-action) #false))
        (spec-seed-copy seed (downfold name (spec-seed-datum seed)) (cons name (spec-seed-namepath seed))
                        #:exceptions (cons maybe-exn (spec-seed-exceptions seed))))
      
      (define (upfold-feature [name : String] [pre-action : (-> Any)] [post-action : (-> Any)] [seed : (Spec-Seed s)] [children-seed : (Spec-Seed s)]) : (Spec-Seed s)
        (with-handlers ([exn:fail? (λ [[e : exn]] (eprintf "[#:after] ~a" (exn-message e)))]) (post-action))
        (spec-seed-copy children-seed
                        (upfold name (spec-seed-datum seed) (spec-seed-datum children-seed))
                        (cdr (spec-seed-namepath children-seed))
                        #:exceptions (cdr (spec-seed-exceptions children-seed))))
      
      (define (fold-behavior [name : String] [action : (-> Void)] [seed : (Spec-Seed s)]) : (Spec-Seed s)
        (define fixed-action : (-> Void)
          (cond [(findf exn? (spec-seed-exceptions seed))
                 => (lambda [[e : exn:fail]] (λ [] (spec-misbehave e)))]
                [else action]))
        (define namepath : (Listof String) (spec-seed-namepath seed))
        (define issue : Spec-Issue
          (parameterize ([default-spec-issue-indention (length namepath)])
            (prove name namepath fixed-action)))
        (spec-seed-copy seed (herefold issue (spec-seed-datum seed)) namepath
                        #:summary (hash-update (spec-seed-summary seed) (spec-issue-type issue) add1 (λ [] 0))))
    
      (spec-seed-summary (spec-behaviors-fold downfold-feature upfold-feature fold-behavior (make-spec-seed seed:datum) feature)))))

(define spec-prove : (->* ((U Spec-Feature Spec-Behavior))
                          (Spec-Prove-Behavior #:fgcolor (-> Spec-Issue-Type Symbol) #:sign (-> Spec-Issue-Type (U Char String)))
                          Spec-Summary)
  (lambda [feature [prove spec-prove-behavior] #:fgcolor [~fgcolor spec-issue-fgcolor] #:sign [~moji spec-issue-moji]]
    (parameterize ([default-spec-handler void])
      (define (downfold-feature [name : String] [seed:orders : (Listof Natural)]) : (Listof Natural)
        (cond [(null? seed:orders) (echof #:fgcolor 'darkgreen #:attributes '(dim underline) "~a~n" name)]
              [else (echof "~a~a ~a~n" (~space (* (length seed:orders) 2))
                           (string-join (map number->string (reverse seed:orders)) ".") name)])
        (cons 1 seed:orders))
      
      (define (upfold-feature [name : String] [whocares : (Listof Natural)] [children:orders : (Listof Natural)]) : (Listof Natural)
        (cond [(< (length children:orders) 2) null]
              [else (cons (add1 (cadr children:orders))
                          (cddr children:orders))]))
      
      (define (fold-behavior [issue : Spec-Issue] [seed:orders : (Listof Natural)]) : (Listof Natural)
        (define type : Spec-Issue-Type (spec-issue-type issue))
        (define headline : String
          (format "~a~a ~a - " (~space (* (spec-issue-indention issue) 2))
            (~moji type) (if (null? seed:orders) 1 (car seed:orders))))
        (define headspace : String (~space (string-length headline)))
        (echof #:fgcolor (~fgcolor type) "~a~a~n" headline (spec-issue-brief issue))
        (case type
          [(misbehaved) (spec-issue-misbehavior-display issue #:indent headspace)]
          [(todo) (spec-issue-todo-display issue #:indent headspace)]
          [(skip) (spec-issue-skip-display issue #:indent headspace)]
          [(panic) (spec-issue-error-display issue #:indent headspace)])
        (if (null? seed:orders) null (cons (add1 (car seed:orders)) (cdr seed:orders))))

      (define-values (box cpu real gc)
        (time-apply (λ [] ((inst spec-summary-fold (Listof Natural))
                           feature null
                           #:downfold downfold-feature #:upfold upfold-feature #:herefold fold-behavior
                           #:prove prove))
                    null))

      (define summary : Spec-Summary (car box))
      (define population : Natural (apply + (hash-values summary)))

      (if (positive? population)
          (let ([~s (λ [[ms : Natural]] : String (~r (* ms 0.001) #:precision '(= 3)))]
                [success (hash-ref summary 'pass (λ [] 0))]
                [misbehavior (hash-ref summary 'misbehaved (λ [] 0))]
                [panic (hash-ref summary 'panic (λ [] 0))]
                [todo (hash-ref summary 'todo (λ [] 0))]
                [skip (hash-ref summary 'skip (λ [] 0))])
            (echof #:fgcolor 'lightcyan "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                   (~s real) (~s (max (- cpu gc) 0)) (~s gc) (~s cpu))
            (echof #:fgcolor 'lightcyan "~a, ~a, ~a, ~a, ~a, ~a% Okay.~n"
                   (~n_w population "sample") (~n_w misbehavior "misbehavior")
                   (~n_w panic "panic") (~n_w skip "skip") (~n_w todo "TODO")
                   (~r #:precision '(= 2) (/ (* (+ success skip) 100) population))))
          (echof #:fgcolor 'darkcyan "~nNo particular example!~n"))

      summary)))
