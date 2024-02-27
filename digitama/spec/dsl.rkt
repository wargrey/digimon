#lang typed/racket/base

(provide (except-out (all-defined-out) describe:it describe:let describe:let* describe:for describe:for* 
                     it:$! spec-format spec-conditional-format spec-parameterize spec-context-with spec-it-with))
(provide (rename-out [define-scenario define-feature]))
(provide (rename-out [describe context]))

(require racket/stxparam)

(require "behavior.rkt")
(require "prompt.rkt")
(require "issue.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-parameter for/spec
  (lambda [stx]
    (raise-syntax-error 'for/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter for*/spec
  (lambda [stx]
    (raise-syntax-error 'for*/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter let/spec
  (lambda [stx]
    (raise-syntax-error 'let/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter let*/spec
  (lambda [stx]
    (raise-syntax-error 'let*/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter let-values/spec
  (lambda [stx]
    (raise-syntax-error 'let-values/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter let*-values/spec
  (lambda [stx]
    (raise-syntax-error 'let*-values/spec "should be inside `describe` or `context`" stx)))

(define-syntax-parameter it
  (lambda [stx]
    (raise-syntax-error 'it "should be inside `describe` or `context`" stx)))

(define-syntax-parameter $!
  (lambda [stx]
    (raise-syntax-error '$! "should be inside `it`" stx)))

(define-syntax (define-scenario stx)
  (syntax-parse stx
    [(_ id:id expr ...)
     (syntax/loc stx (define id : Spec-Feature (describe 'id expr ...)))]))

(define-syntax (describe stx)
  (syntax-parse stx
    [(_ [fmt:str brief ...] rest ...) (syntax/loc stx (describe (format fmt brief ...) rest ...))]
    [(_ brief (~optional (~seq #:do
                               (~alt (~optional (~seq #:before setup))
                                     (~optional (~seq #:after teardown))
                                     (~optional (~seq #:parameterize clause))) ...))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)]
                   [pclause (or (attribute clause) #'[])])
       (syntax/loc stx
         (make-spec-feature brief
                            (syntax-parameterize ([it (make-rename-transformer #'describe:it)]
                                                  [let/spec (make-rename-transformer #'describe:let)]
                                                  [let*/spec (make-rename-transformer #'describe:let*)]
                                                  [let-values/spec (make-rename-transformer #'describe:let-values)]
                                                  [let*-values/spec (make-rename-transformer #'describe:let*-values)]
                                                  [for/spec (make-rename-transformer #'describe:for)]
                                                  [for*/spec (make-rename-transformer #'describe:for*)])
                              (list (λ [] expr) #| the simplest way to do lazy evaluation |# ...))
                            #:before setup #:after teardown #:parameterize (spec-parameterize pclause))))]))

(define-syntax (describe:it stx)
  (syntax-parse stx
    [(_ (~seq cbrief0 (~and (~or #:when #:unless) kw-cond0) condition0) ... #:do rest ...) (raise-syntax-error 'describe:it "need an unconditional description" stx)]
    [(_ (~seq cbrief0 (~and (~or #:when #:unless) kw-cond0) condition0) (~seq cbrief (~and (~or #:when #:unless) kw-cond) condition) ... brief rest ...)
     (syntax/loc stx
       (describe:it (or (spec-conditional-format cbrief0 kw-cond0 condition0)
                        (spec-conditional-format cbrief kw-cond condition) ...
                        (spec-format brief))
                    rest ...))]
    [(_it brief (~optional (~seq #:do
                                 (~alt (~optional (~seq #:before setup))
                                       (~optional (~seq #:after teardown))
                                       (~optional (~seq #:parameterize clause))
                                       (~optional (~seq (~or #:timeout/ms #:millisecond) timeout))) ...))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)]
                   [pclause (or (attribute clause) #'[])]
                   [timeout (or (attribute timeout) #'0)]
                   [empty? (null? (syntax->list #'(expr ...)))])
       (syntax/loc stx
         (if (and empty?)
             (make-spec-behavior (spec-format brief)
                                 (λ [] (parameterize ([default-spec-issue-locations (cons #'_it (default-spec-issue-locations))])
                                         (spec-misbehave 'todo))))
             (make-spec-behavior (spec-format brief)
                                 (syntax-parameterize ([$! (make-rename-transformer #'it:$!)])
                                   (λ [] expr ... (void))) ; this is lazy by nature
                                 #:before setup #:after teardown #:parameterize (spec-parameterize pclause)
                                 #:timeout timeout))))]))

(define-syntax (describe:for stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (for/list : (Listof (U Spec-Feature Spec-Behavior)) (clauses ...) body ...))]))

(define-syntax (describe:for* stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (for*/list : (Listof (U Spec-Feature Spec-Behavior)) (clauses ...) body ...))]))

(define-syntax (describe:let stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (let (clauses ...) (list body ...)))]))

(define-syntax (describe:let* stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (let* (clauses ...) (list body ...)))]))

(define-syntax (describe:let-values stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (let-values (clauses ...) (list body ...)))]))

(define-syntax (describe:let*-values stx)
  (syntax-parse stx
    [(_ (clauses ...) body ...)
     (syntax/loc stx
       (let*-values (clauses ...) (list body ...)))]))

; speaking of the original location
; using it directly causes recursively macro expanding
(define-syntax (define-behavior stx)
  (syntax-parse stx
    [(_ [id pstx ...] (local:expr ([defs:expr ...] ...) [#:it brief ... #:do body ...] ...))
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-behavior b-stx)])
              (syntax/loc b-stx (local ([defs ...] ...) (list (spec-it-with loc #:it brief ... #:do body ...) ...))))])))]
    [(_ [id pstx ...] [#:it brief ... #:do body ...] ...)
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-behavior b-stx)])
              (syntax/loc b-stx
                (list (spec-it-with loc #:it brief ... #:do body ...) ...)))])))]
    [(_ [id pstx ...] (local:expr ([defs:expr ...] ...) #:it brief ... #:do body ...))
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-behavior b-stx)])
              (syntax/loc b-stx (local ([defs ...] ...) (spec-it-with loc #:it brief ... #:do body ...))))])))]
    [(_ [id pstx ...] #:it brief ... #:do body ...)
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-behavior b-stx)])
              (syntax/loc b-stx
                (spec-it-with loc #:it brief ... #:do body ...)))])))]))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ [id pstx ...] (local:expr ([defs:expr ...] ...) #:desc brief ... #:do body ...))
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-context b-stx)])
              (syntax/loc b-stx
                (local ([defs ...] ...)
                  (spec-context-with loc #:desc brief ... #:do body ...))))])))]
    [(_ [id pstx ...] #:desc brief ... #:do body ...)
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false 'define-context b-stx)])
              (syntax/loc b-stx
                (spec-context-with loc #:desc brief ...  #:do body ...)))])))]))

(define-syntax (it:$! stx)
  (syntax-parse stx
    [(_ var:id expr:expr)
     (syntax/loc stx
       (let ([v expr])
         (set! var v)
         v))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (ignore stx)
  (syntax-parse stx
    [(_ reason:str argl ...)
     (syntax/loc stx
       (raise (make-exn:fail:unsupported
               (spec-message reason (list argl ...))
               (current-continuation-marks))))]))

(define-syntax (pending stx)
  (syntax-parse stx
    [(id reason:str argl ...)
     (syntax/loc stx
       (parameterize ([default-spec-issue-message (spec-message reason (list argl ...))]
                      [default-spec-issue-locations (cons #'id (default-spec-issue-locations))])
         (spec-misbehave 'todo)))]))

(define-syntax (collapse stx)
  (syntax-parse stx
    [(id reason:str argl ...)
     (syntax/loc stx
       (parameterize ([default-spec-issue-message (spec-message reason (list argl ...))]
                      [default-spec-issue-locations (cons #'id (default-spec-issue-locations))])
         (spec-misbehave)))]))

(define-syntax (make-it stx)
  (syntax-parse stx
    [(_ argl ...)
     (syntax/loc stx
       #true)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (spec-format stx)
  (syntax-parse stx
    [(_ [fmt:str brief ...]) (syntax/loc stx (format fmt brief ...))]
    [(_ brief) (syntax/loc stx brief)]))

(define-syntax (spec-conditional-format stx)
  (syntax-parse stx
    [(_ brief #:when condition) (syntax/loc stx (and condition (spec-format brief)))]
    [(_ brief #:unless condition) (syntax/loc stx (and (not condition) (spec-format brief)))]
    [(_ brief) (syntax/loc stx (spec-format brief))]))

(define-syntax (spec-parameterize stx)
  (syntax-parse stx
    [(_ []) (syntax/loc stx #false)]
    [(_ ([p0 v0] [p v] ...)) (syntax/loc stx (λ [] (p0 v0) (p v) ... (void)))]))

(define-syntax (spec-context-with stx)
  (syntax-parse stx
    [(_ loc-stx #:desc brief ... #:do body ...)
     (syntax/loc stx
       (describe brief ... #:do
                 #:parameterize ([default-spec-issue-locations (cons #'loc-stx (default-spec-issue-locations))]) #:do
                 body ...))]))

(define-syntax (spec-it-with stx)
  (syntax-parse stx
    [(_ loc-stx #:it brief ... #:do body ...)
     (syntax/loc stx
       (it brief ... #:do
           (parameterize ([default-spec-issue-locations (cons #'loc-stx (default-spec-issue-locations))])
             body ...)))]))
