#lang typed/racket/base

(provide (except-out (all-defined-out) describe:it it:$!))
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
                                     (~optional (~seq #:after teardown))) ...))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)])
       (syntax/loc stx
         (make-spec-feature brief
                            (syntax-parameterize ([it (make-rename-transformer #'describe:it)])
                              (list (λ [] (spec-expand expr)) #| the simplest way to do lazy evaluation |# ...))
                            #:before setup #:after teardown)))]))

(define-syntax (describe:it stx)
  (syntax-parse stx
    [(_ [fmt:str brief ...] rest ...) (syntax/loc stx (describe:it (format fmt brief ...) rest ...))]
    [(_ brief (~optional (~seq #:do
                               (~alt (~optional (~seq #:before setup))
                                     (~optional (~seq #:after teardown))
                                     (~optional (~seq (~or #:timeout/ms #:millisecond) timeout))) ...))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)]
                   [timeout (or (attribute timeout) #'0)]
                   [empty? (null? (syntax->list #'(expr ...)))])
       (syntax/loc stx
         (if (and empty?)
             (make-spec-behavior brief
                                 (λ [] (parameterize ([default-spec-issue-location (or (default-spec-issue-location) (spec-location #'brief))])
                                         (spec-misbehave 'todo))))
             (make-spec-behavior brief
                                 (syntax-parameterize ([$! (make-rename-transformer #'it:$!)])
                                   (λ [] expr ... (void))) ; this is lazy by nature
                                 #:before setup #:after teardown
                                 #:timeout timeout))))]))

(define-syntax (define-behavior stx)
  (syntax-parse stx
    [(_ [id pstx ...] (local ... #:it brief #:do body ...))
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" b-stx)])
              (syntax/loc b-stx
                (local ...
                  (it brief #:do
                      (parameterize ([default-spec-issue-location (or (default-spec-issue-location) (spec-location #'loc))])
                        body ...)))))])))]
    [(_ [id pstx ...] definition ... #:it brief #:do body ...)
     (syntax/loc stx
       (define-syntax (id b-stx)
         (syntax-parse b-stx
           [(_ pstx ...)
            (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" b-stx)])
              (syntax/loc b-stx
                (begin definition ...
                       (it brief #:do
                           (parameterize ([default-spec-issue-location (or (default-spec-issue-location) (spec-location #'loc))])
                             body ...)))))])))]))

(define-syntax (spec-expand stx)
  (syntax-case stx [for for*]
    [(_ (for (clause ...) body ...)) (syntax/loc stx (for/list : (Listof (U Spec-Feature Spec-Behavior)) (clause ...) body ...))]
    [(_ (for* (clause ...) body ...)) (syntax/loc stx (for*/list : (Listof (U Spec-Feature Spec-Behavior)) (clause ...) body ...))]
    [(_ expr ...) (syntax/loc stx (begin expr ...))]))

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
                      [default-spec-issue-location (or (default-spec-issue-location) (spec-location #'id))])
         (spec-misbehave 'todo)))]))

(define-syntax (collapse stx)
  (syntax-parse stx
    [(id reason:str argl ...)
     (syntax/loc stx
       (parameterize ([default-spec-issue-message (spec-message reason (list argl ...))]
                      [default-spec-issue-location (or (default-spec-issue-location) (spec-location #'id))])
         (spec-misbehave)))]))

(define-syntax (make-it stx)
  (syntax-parse stx
    [(_ argl ...)
     (syntax/loc stx
       #true)]))
