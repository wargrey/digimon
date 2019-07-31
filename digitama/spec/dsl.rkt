#lang typed/racket/base

(provide define-feature describe)

(require racket/stxparam)

(require "behavior.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-parameter it
  (lambda [stx]
    (raise-syntax-error 'it "cannot be used outside `describe` or `context`" stx)))

(define-syntax (define-feature stx)
  (syntax-parse stx
    [(_ id:id expr ...)
     #'(define id : Spec-Feature
         (describe 'id expr ...))]))

(define-syntax (describe stx)
  (syntax-parse stx
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (if (attribute setup) #'setup #'void)]
                   [teardown (if (attribute teardown) #'teardown #'void)])
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
    [(_ (expect expr ...)) (raise-syntax-error 'spec "cannot use expectation directly" stx)]))

(define-syntax (it:describe stx)
  (syntax-parse stx #:literals [:]
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (if (attribute setup) #'setup #'void)]
                   [teardown (if (attribute teardown) #'teardown #'void)])
       #'(make-spec-behavior brief (λ [] (void expr ...))
                             #:before setup #:after teardown))]))
