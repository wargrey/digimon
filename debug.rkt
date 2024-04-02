#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)
(require racket/format)

(require "digitama/minimal/format.rkt")
(require "echo.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw) (syntax/loc stx (assert* sexp pred throw #false 'assert))]
    [(_ sexp pred throw src0 srcs ...)
     (quasisyntax/loc stx
       (let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw src0 srcs ... ? v)))
            'feature-profile:TR-dynamic-check #t)))]))

(define-syntax (time* stx)
  (syntax-case stx []
    [(_ #:title title body ...)
     (syntax/loc stx
       (let-values ([(retval memory cpu real gc) (time-apply* (λ [] body ...))])
         (echof #:fgcolor 208 "~amemory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                (let ([t (~a title)]) (if (string=? t "") "" (format "~a: " t)))
                (~size memory) (~gctime cpu) (~gctime real) (~gctime gc))
         retval))]
    [(_ body ...) (syntax/loc stx (time* #:title '|| body ...))]))

(define-syntax (time** stx)
  (syntax-case stx []
    [(_ body ...)
     (syntax/loc stx
       (begin (collect-garbage*)
              (time* body ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tee : (All (a) (-> a [#:printer (-> Any Output-Port Any)] Output-Port * a))
  (lambda [v #:printer [<< pretty-print] . outs]
    (for ([out (in-list (cons (current-output-port) outs))])
      (<< v out))
    v))

(define time-apply* : (All (a) (-> (-> a) (Values a Integer Natural Natural Natural)))
  (lambda [do-task]
    (define memory0 : Natural (current-memory-use 'cumulative))
    (define-values (retval cpu real gc) (time-apply do-task null))
    (define memory : Integer (- (current-memory-use 'cumulative) memory0))

    (values (car retval) memory cpu real gc)))

(define time-apply** : (All (a) (-> (-> a) (Values a Integer Natural Natural Natural)))
  (lambda [do-task]
    (collect-garbage*)
    (time-apply* do-task)))

(define collect-garbage* : (-> Void)
  (lambda []
    (collect-garbage 'major)
    (collect-garbage 'major)
    (collect-garbage 'major)))
