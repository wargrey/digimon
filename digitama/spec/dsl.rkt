#lang typed/racket/base

(provide describe)

(require "behavior.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (describe stx)
  (syntax-parse stx #:literals [:]
    [(_ [brief:str] expr ...)
     #'(describe brief expr ...)]
    [(_ [brfmt:str argl ...] expr ...)
     #'(let ([brief (format brfmt argl ...)])
         (describe brief expr ...))]
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (if (attribute setup) #'setup #'void)]
                   [teardown (if (attribute teardown) #'teardown #'void)])
       #'(make-spec-feature brief (list expr ...)
                            #:before setup #:after teardown))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (context stx)
  (syntax-case stx [:]
    [(_ expr ...)
     #'(describe expr ...)]))

(define-syntax (it stx)
  (syntax-parse stx #:literals [:]
    [(_ [brief:str] expr ...)
     #'(it brief expr ...)]
    [(_ [brfmt:str argl ...] expr ...)
     #'(let ([brief (format brfmt argl ...)])
         (it brief expr ...))]
    [(_ brief (~optional (~seq #:do
                               (~or (~seq (~optional (~seq #:before setup)) (~optional (~seq #:after teardown)))
                                    (~seq (~seq #:after teardown) (~seq #:before setup)))))
        (~seq #:do expr ...))
     (with-syntax ([setup (if (attribute setup) #'setup #'void)]
                   [teardown (if (attribute teardown) #'teardown #'void)])
       #'(make-spec-behavior brief (λ [] (void expr ...))
                             #:before setup #:after teardown))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "expectation.rkt")

(describe "Prelude" #:do
          (describe ["~a" (object-name read)] #:do
                    (context "when provided with invalid value" #:do
                             (it ["returns a ~a error" 'parse] #:do
                                 (expect-collapse "parse error"))))
          
          (describe ["~a" (object-name car)] #:do
                    (it "return the first element of a list" #:do)))
