#lang typed/racket/base

(provide (all-defined-out))
(provide wisemon-spec? wisemon-spec-ref)
(provide Wisemon-Spec Wisemon-Specification)

(require racket/future)

(require "digitama/wisemon.rkt")

(require "dtrace.rkt")
(require "filesystem.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (wisemon-spec stx)
  (syntax-parse stx #:datum-literals [: :-]
    [(_ target (~or : #:) prerequisites (~or :- #:-) expr ...)
     #'(unsafe-wisemon-spec target prerequisites
                            (λ [] expr ... (void))
                            (make-special-comment (list 'expr ...)))]
    [(_ target (~or :- #:-) expr ...)
     #'(wisemon-spec target #: null #:- expr ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-make : (->* (Wisemon-Specification)
                            ((Listof Path-String)
                             #:keep-going? Boolean #:dry-run? Boolean #:always-run? Boolean #:just-touch? Boolean
                             #:jobs Positive-Integer #:name Symbol #:cd Path-String
                             #:assume-old (Listof Path-String) #:assume-new (Listof Path-String))
                            Void)
  (lambda [specs [targets null]
                 #:keep-going? [keep-going? #false] #:dry-run? [dry-run? #false] #:always-run? [always-run? #false] #:just-touch? [just-touch? #false]
                 #:jobs [jobs (processor-count)] #:name [the-name 'wisemon] #:cd [cd (current-directory)]
                 #:assume-old [oldfiles null] #:assume-new [newfiles null]]
    (when (pair? specs)
      (parameterize ([current-directory cd]
                     [current-logger (make-logger the-name (current-logger))])
        (define phony-all : (Listof Path) (if (pair? targets) (map simple-form-path targets) (wisemon-targets-flatten specs)))
        (for ([t (in-list (if (pair? targets) (map simple-form-path targets) (wisemon-targets-flatten specs)))])
          (with-handlers ([exn:wisemon? (λ [[e : exn:wisemon]] (if (not keep-going?) (raise e) (dtrace-exception e #:level 'warning #:topic the-name)))])
            (wisemon-make-target specs t the-name dry-run? always-run? just-touch?
                                 (map simple-form-path oldfiles)
                                 (map simple-form-path newfiles))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wisemon-targets-flatten : (-> Wisemon-Specification (Listof Path))
  (lambda [specs]
    (for/fold ([targets : (Listof Path) null])
              ([spec (in-list specs)])
      (define ts (wisemon-spec-target spec))
      (cond [(list? ts) (append targets ts)]
            [else (append targets (list ts))]))))
