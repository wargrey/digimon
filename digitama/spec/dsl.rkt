#lang typed/racket/base

(provide define-scenario describe)
(provide (rename-out [define-scenario define-feature]))

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
    (raise-syntax-error 'it "cannot be used outside `describe` or `context`" stx)))

(define-syntax (define-scenario stx)
  (syntax-parse stx
    [(_ id:id expr ...)
     #'(define id : Spec-Feature (describe 'id expr ...))]))

(define-syntax (describe stx)
  (syntax-parse stx
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)])
       #'(make-spec-feature brief
                            (syntax-parameterize ([it (make-rename-transformer #'it:describe)])
                              (list (spec-expand expr) ...))
                            #:before setup #:after teardown))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (spec-expand stx)
  (syntax-parse stx #:datum-literals [describe context it]
    [(_ (describe expr ...)) #'(describe expr ...)]
    [(_ (context expr ...)) #'(describe expr ...)]
    [(_ (it expr ...)) #'(it expr ...)]
    [(_ (expect expr ...)) (raise-syntax-error 'spec "unrecognized clause" #'expect)]))

(define-syntax (it:describe stx)
  (syntax-parse stx
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (or (attribute setup) #'void)]
                   [teardown (or (attribute teardown) #'void)]
                   [empty? (null? (syntax->list #'(expr ...)))])
       #'(if (and empty?)
             (make-spec-behavior brief (λ [] (parameterize ([default-spec-issue-location (spec-location #'brief)])
                                               (spec-misbehave 'todo))))
             (make-spec-behavior brief (λ [] expr ... (void))
                                 #:before setup #:after teardown)))]))
