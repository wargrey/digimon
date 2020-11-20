#lang typed/racket/base

(provide (all-defined-out))

(require "format.rkt")
(require "echo.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw) #'(assert* sexp pred throw 'assert)]
    [(_ sexp pred throw src)
     #`(let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw src (~a (object-name ?)) v)))
            'feature-profile:TR-dynamic-check #t))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tee : (All (a) (-> a [#:printer (-> Any Output-Port Any)] Output-Port * a))
  (lambda [v #:printer [<< pretty-print] . outs]
    (for ([out (in-list (cons (current-output-port) outs))]) (<< v out))
    v))

(define time-apply* : (All (a) (-> (-> a) a))
  (lambda [do-task]
    (define-values (retval cpu real gc) (time-apply do-task null))
    (echof #:fgcolor 208 "cpu time: ~a real time: ~a gc time: ~a~n" (~gctime cpu) (~gctime real) (~gctime gc))
    (car retval)))
