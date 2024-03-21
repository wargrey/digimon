#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [spec-begin example-begin]
                     [spec-begin begin-spec]
                     [spec-begin begin-example]))

(provide Spec-Summary Spec-Behavior Spec-Feature Spec-Issue-Format Spec-Issue-Format-Datum)
(provide spec-feature? spec-behavior? spec-feature-brief default-spec-issue-handler)
(provide make-spec-behavior make-spec-feature spec-behaviors-fold)
(provide define-feature define-scenario define-behavior define-context describe context)
(provide let/spec let*/spec let-values/spec let*-values/spec for/spec for*/spec it $!)
(provide collapse ignore pending make-it spec-misbehave (rename-out [make-it pass]))
(provide spec-format/octet spec-format/bin spec-format/hex)

(provide (all-from-out "digitama/spec/issue.rkt"))
(provide (all-from-out "digitama/spec/expectation.rkt"))
(provide (all-from-out "digitama/spec/expect/logging.rkt"))
(provide (all-from-out "digitama/spec/expect/exec.rkt"))

(require "digitama/spec/issue.rkt")
(require "digitama/spec/prompt.rkt")
(require "digitama/spec/seed.rkt")
(require "digitama/spec/misc.rkt")

(require "digitama/spec/expectation.rkt")
(require "digitama/spec/behavior.rkt")
(require "digitama/spec/dsl.rkt")

(require "digitama/spec/expect/logging.rkt")
(require "digitama/spec/expect/exec.rkt")

(require "format.rkt")
(require "debug.rkt")
(require "echo.rkt")

(require racket/string)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (spec-begin stx)
  (syntax-parse stx
    [(_ id:id #:do expr ...)
     (syntax/loc stx
       (begin (define-feature id #:do expr ...)

              (void ((default-spec-handler) 'id id))))]
    [(_ id:id expr ...) (syntax/loc stx (begin (spec-begin id #:do expr ...)))]
    [(_ expr ...) (syntax/loc stx (begin (spec-begin spec #:do expr ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Behavior-Prove (-> String (Listof String) (-> Void) Spec-Issue))
(define-type Spec-Issue-Fgcolor (-> Spec-Issue-Type Symbol))
(define-type Spec-Issue-Symbol (-> Spec-Issue-Type (U Char String)))
(define-type Spec-Prove-Pattern (U String Regexp '*))
(define-type Spec-Prove-Selector (Listof Spec-Prove-Pattern))

(define spec-behavior-prove : Spec-Behavior-Prove
  (lambda [brief namepath evaluation]
    (spec-story (gensym brief)
                (λ [] (parameterize ([default-spec-issue-brief brief])
                        (with-handlers ([exn:fail? spec-misbehave])
                          (evaluation)
                          (make-spec-issue 'pass)))))))

(define default-spec-handler : (Parameterof (-> Symbol Spec-Feature Any)) (make-parameter (λ [[id : Symbol] [spec : Spec-Feature]] (spec-prove spec))))
(define default-spec-behavior-prove : (Parameterof Spec-Behavior-Prove) (make-parameter spec-behavior-prove))
(define default-spec-issue-fgcolor : (Parameterof Spec-Issue-Fgcolor) (make-parameter spec-issue-fgcolor))
(define default-spec-issue-symbol : (Parameterof Spec-Issue-Symbol) (make-parameter spec-issue-moji))

(define default-spec-no-timing-info : (Parameterof Boolean) (make-parameter #false))
(define default-spec-no-location-info : (Parameterof Boolean) (make-parameter #false))
(define default-spec-no-argument-expression : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-summary-fold
  : (All (s) (-> (U Spec-Feature Spec-Behavior) s
                 #:downfold (-> String Index s s) #:upfold (-> String Index s s s) #:herefold (-> String Spec-Issue Index Integer Natural Natural Natural s s)
                 [#:selector Spec-Prove-Selector]
                 (Pairof Spec-Summary s)))
  (lambda [feature seed:datum #:downfold downfold #:upfold upfold #:herefold herefold #:selector [selector null]]
    (define prove : Spec-Behavior-Prove (default-spec-behavior-prove))
    (define selectors : (Vectorof Spec-Prove-Pattern) (list->vector selector))
    (define selcount : Index (vector-length selectors))
    
    (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines from shutting down the current custodian accidently.
      (define (downfold-feature [brief : String] [pre-action : (-> Any)] [post-action : (-> Any)] [seed : (Spec-Seed s)]) : (Option (Spec-Seed s))
        (define namepath : (Listof String) (spec-seed-namepath seed))
        (define cursor : Index (length namepath))

        (and (or (>= cursor selcount)
                 (let ([pattern (vector-ref selectors cursor)])
                   (cond [(eq? pattern '*)]
                         [(string? pattern) (string=? pattern brief)]
                         [else (regexp-match? pattern brief)])))

             (let ([maybe-exn (with-handlers ([exn:fail? (λ [[e : exn:fail]] e)]) (pre-action) #false)])
               (spec-seed-copy #:exceptions (cons maybe-exn (spec-seed-exceptions seed))
                               seed (downfold brief (length namepath) (spec-seed-datum seed)) (cons brief namepath)))))
      
      (define (upfold-feature [brief : String] [pre-action : (-> Any)] [post-action : (-> Any)] [seed : (Spec-Seed s)] [children-seed : (Spec-Seed s)]) : (Spec-Seed s)
        (with-handlers ([exn:fail? (λ [[e : exn]] (eprintf "[#:after] ~a" (exn-message e)))]) (post-action))
        (spec-seed-copy #:exceptions (cdr (spec-seed-exceptions children-seed))
                        children-seed
                        (upfold brief (length (spec-seed-namepath children-seed)) (spec-seed-datum seed) (spec-seed-datum children-seed))
                        (cdr (spec-seed-namepath children-seed))))
      
      (define (fold-behavior [brief : String] [action : (-> Void)] [timeout : Natural] [seed : (Spec-Seed s)]) : (Spec-Seed s)
        (define fixed-action : (-> Void)
          (cond [(findf exn? (spec-seed-exceptions seed))
                 => (λ [[e : exn:fail]] (λ [] (spec-misbehave e)))]
                [(> timeout 0)
                 (λ [] (let ([ghostcat (thread action)])
                         (with-handlers ([exn:fail? (λ [[e : exn:fail]] (kill-thread ghostcat) (spec-misbehave e))]
                                         [exn:break? (λ [[e : exn:break]] (kill-thread ghostcat) (raise e))])
                           (unless (sync/timeout/enable-break (/ timeout 1000.0) (thread-dead-evt ghostcat))
                             (error 'spec "timeout (longer than ~a)" (~gctime timeout))))))]
                [else action]))
        (define namepath : (Listof String) (spec-seed-namepath seed))
        (define-values (issue memory cpu real gc) (time-apply* (λ [] (prove brief namepath fixed-action))))

        (spec-seed-copy #:summary (hash-update (spec-seed-summary seed) (spec-issue-type issue) add1 (λ [] 0))
                        seed (herefold brief issue (length namepath) memory cpu real gc (spec-seed-datum seed)) namepath))

      (let ([s (spec-behaviors-fold downfold-feature upfold-feature fold-behavior (make-spec-seed seed:datum) feature)])
        (cons (spec-seed-summary s) (spec-seed-datum s))))))

(define spec-prove : (->* ((U Spec-Feature Spec-Behavior))
                          (#:selector Spec-Prove-Selector #:no-summary? Boolean
                           #:no-timing-info? Boolean #:no-location-info? Boolean #:no-argument-expression? Boolean)
                          Natural)
  (lambda [#:selector [selector null] #:no-summary? [no-summary? #false]
           #:no-timing-info? [no-timing-info? (default-spec-no-timing-info)]
           #:no-location-info? [no-loc? (default-spec-no-location-info)]
           #:no-argument-expression? [no-expr? (default-spec-no-argument-expression)]
           feature]
    (define ~fgcolor : Spec-Issue-Fgcolor (default-spec-issue-fgcolor))
    (define ~symbol : Spec-Issue-Symbol (default-spec-issue-symbol))
    
    (parameterize ([default-spec-issue-handler void])
      (define (downfold-feature [brief : String] [indent : Index] [seed:orders : (Listof Natural)]) : (Listof Natural)
        (cond [(= indent 0) (echof #:fgcolor 'darkgreen #:attributes '(dim underline) "~a~n" brief)]
              [else (echof "~a~a ~a~n" (~space (+ indent indent)) (string-join (map number->string (reverse seed:orders)) ".") brief)])
        (cons 1 seed:orders))
      
      (define (upfold-feature [brief : String] [indent : Index] [who-cares : (Listof Natural)] [children:orders : (Listof Natural)]) : (Listof Natural)
        (cond [(< indent 2) null]
              [else (cons (add1 (cadr children:orders))
                          (cddr children:orders))]))
      
      (define (fold-behavior [brief : String] [issue : Spec-Issue] [indent : Index]
                             [memory : Integer] [cpu : Natural] [real : Natural] [gc : Natural] [seed:orders : (Listof Natural)]) : (Listof Natural)
        (define type : Spec-Issue-Type (spec-issue-type issue))
        (define headline : String (format "~a~a ~a - " (~space (+ indent indent)) (~symbol type) (if (null? seed:orders) 1 (car seed:orders))))
        (define headspace : String (~space (string-utf-8-length headline)))
        (define briefs : (Pairof String (Listof String)) (~string-lines (or (spec-issue-brief issue) "")))
        (define fgc : Symbol (~fgcolor type))

        (echof #:fgcolor fgc "~a~a" headline (car briefs))

        (when (and (eq? type 'pass) (not no-timing-info?))
          (echof #:fgcolor 'darkgrey " [~a, ~a task time]" (~size memory) (~gctime (- real gc))))

        (newline)
        (for ([subline (in-list (cdr briefs))])
          (echof #:fgcolor fgc "~a~a~n" headspace subline))

        (case type
          [(misbehaved) (spec-issue-misbehavior-display issue #:indent headspace #:no-location? no-loc? #:no-argument-expression? no-expr?)]
          [(todo) (spec-issue-todo-display issue #:indent headspace #:no-location? no-loc?)]
          [(skip) (spec-issue-skip-display issue #:indent headspace)]
          [(panic) (spec-issue-error-display issue #:indent headspace)])

        (if (null? seed:orders) null (cons (add1 (car seed:orders)) (cdr seed:orders))))

      (define-values (summaries memory cpu real gc)
        (time-apply* (λ [] ((inst spec-summary-fold (Listof Natural))
                            feature null
                            #:downfold downfold-feature #:upfold upfold-feature #:herefold fold-behavior
                            #:selector selector))))

      (define summary : Spec-Summary (car summaries))
      (define population : Natural (apply + (hash-values summary)))
      (define misbehavior : Natural (hash-ref summary 'misbehaved (λ [] 0)))
      (define panic : Natural (hash-ref summary 'panic (λ [] 0)))
      (define failures : Natural (+ misbehavior panic))

      (when (not no-summary?)
        (if (positive? population)
            (let ([~s (λ [[ms : Natural]] : String (~r (* ms 0.001) #:precision '(= 3)))]
                  [pass (hash-ref summary 'pass (λ [] 0))]
                  [todo (hash-ref summary 'todo (λ [] 0))]
                  [skip (hash-ref summary 'skip (λ [] 0))])
              (echof #:fgcolor 'lightcyan "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                     (~s real) (~s (max (- cpu gc) 0)) (~s gc) (~s cpu))
              (echof #:fgcolor (if (> failures 0) 'lightyellow 'lightcyan) "~a, ~a, ~a, ~a pending, ~a skipped, ~a% Okay.~n"
                     (~n_w population "sample") (~n_w misbehavior "misbehavior") (~n_w panic "panic") todo skip
                     (~r #:precision '(= 2) (/ (* (+ pass skip) 100) population))))
            (echof #:fgcolor 'darkcyan "~nNo particular sample!~n")))

      failures)))
