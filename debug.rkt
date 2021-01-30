#lang typed/racket/base

(provide (all-defined-out))

(require "format.rkt")
(require "echo.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw) (syntax/loc stx (assert* sexp pred throw 'assert))]
    [(_ sexp pred throw src)
     (quasisyntax/loc stx
       (let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw src (~a (object-name ?)) v)))
            'feature-profile:TR-dynamic-check #t)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tee : (All (a) (-> a [#:printer (-> Any Output-Port Any)] Output-Port * a))
  (lambda [v #:printer [<< pretty-print] . outs]
    (for ([out (in-list (cons (current-output-port) outs))]) (<< v out))
    v))

(define time-apply* : (All (a) (->* ((-> a)) (Boolean) a))
  (lambda [do-task [memory? #false]]
    (define memory0 : Natural (current-memory-use 'cumulative))
    (define-values (retval cpu real gc) (time-apply do-task null))
    (define memory : Integer (- (current-memory-use 'cumulative) memory0))

    (echof #:fgcolor 208
           "~acpu time: ~a real time: ~a gc time: ~a~n"
           (if (not memory?) "" (format "memory: ~a "(~size memory)))
           (~gctime cpu) (~gctime real) (~gctime gc))

    (car retval)))

(define collect-garbage* : (-> Void)
  (lambda []
    (collect-garbage 'major)
    (collect-garbage 'major)
    (collect-garbage 'major)))
