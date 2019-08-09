#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/cmdopt.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(require (for-syntax "digitama/cmdopt.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (command-line stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~optional (~seq #:program name) #:defaults ([name #'(find-system-path 'run-file)]))
        (~optional (~seq #:argv arguments) #:defaults ([arguments #'(current-command-line-arguments)]))
        (~optional (~seq #:args args-form (~optional (~seq (~or #: :) Type) #:defaults ([Type #'Void])) body-expr ...) #:defaults ([args-form #'()])))
     (with-syntax* ([(args [<String> ...] [<args> ...] [<idx> ...]) (cmd-parse-args #'args-form)])
       #'(let ([argv : (Vectorof String) arguments])
           (define main : (-> <String> ... Type)
             (lambda args
               (void)
               body-expr ...))

           (writeln '(-> <String> ... Type))
           (writeln (map symbol->string '(<args> ...)))
           (writeln (list '<idx> ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(command-line
 #:program 'nas
 #:argv (vector "ac")
 #:args (src ... dest) #: Flonum
 1.0)
