#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "prompt.rkt")
(require "issue.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

(define-syntax (define-assert stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...) body ...)
     #'(begin (define do-? : (-> Syntax Any (->* (Type ...) (String) #:rest Any Void))
                (λ [stx expression]
                  (define ? : (->* (Type ...) (String) #:rest Any Void)
                    (λ [arg ... . argl]
                      (define message : (Option String)
                        (and (pair? argl) (string? (car argl))
                             (cond [(null? (cdr argl)) (car argl)]
                                   [else (apply format (car argl) (cdr argl))])))
                      ((inst with-issue-info Void)
                       (list (cons 'name '?)
                             (cons 'location (issue-location stx))
                             (cons 'expression expression)
                             (cons 'arguments (list arg ...))
                             (cons 'message message))
                       (λ [] body ... (void)))))
                  ?))

              (define argc : Index (length '(arg ...)))
              
              (define-syntax (? stx)
                (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                  (syntax-parse stx
                    [(?:id . arglist) #'((do-? #'loc (cons '? (take 'arglist argc))) . arglist)]
                    [?:id #'(do-? #'loc '?)]))))]))

(define-syntax (define-boolean-assert stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...) body ...)
     #'(define-assert (? [arg : Type] ...)
         (or (let () (void) body ...)
             (spec-collapse)))]))

(define-syntax (define-binary-assert stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [gv:id : GType:expr] [ev:id : EType:expr]) body ...)
     #'(define-assert (? [gv : GType] [ev : EType])
         (with-issue-info (list (cons 'expected ev)
                                (cons 'given gv))
           (λ [] (or (let () (void) body ...)
                     (spec-collapse)))))]
    [(_ (?:id pred:id [gv:id : GType:expr] [ev:id : EType:expr]))
     #'(define-binary-assert (? [gv : GType] [ev : EType])
         (pred gv ev))]
    [(_ (?:id [pred:id : (-> GType:expr EType:expr Type)]))
     #'(define-binary-assert (? [gv : GType] [ev : EType])
         (pred gv ev))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-assert (%?-eq [v1 : Any] [v2 : Any]) (eq? v1 v2))
(define-binary-assert (%?-eqv [v1 : Any] [v2 : Any]) (eqv? v1 v2))
(define-binary-assert (%?-equal [v1 : Any] [v2 : Any]) (equal? v1 v2))
(define-boolean-assert (%?-!eq [v1 : Any] [v2 : Any]) (not (eq? v1 v2)))
(define-boolean-assert (%?-!eqv [v1 : Any] [v2 : Any]) (not (eqv? v1 v2)))
(define-boolean-assert (%?-!equal [v1 : Any] [v2 : Any]) (not (equal? v1 v2)))

(define-boolean-assert (%?-true [v : Any]) (eq? v #true))
(define-boolean-assert (%?-false [v : Any]) (eq? v #false))
(define-boolean-assert (%?-!false [v : Any]) v)
(define-boolean-assert (%?-#) #false)

(define-boolean-assert (%?-fl= [v1 : Flonum] [v2 : Flonum] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- v1 v2)) epsilon))

(define-assert (%?-exn [expt : (U (-> Any Boolean) (U Byte-Regexp Regexp Bytes String))] [do-task : (-> Any)])
  (define maybe-e (with-handlers ([exn:fail? values]) (void (do-task))))
  (let ([e? (if (procedure? expt) expt exn:fail?)])
    (cond [(and (exn:fail? maybe-e) (e? maybe-e))
           (or (procedure? expt)
               (regexp-match? expt (exn-message maybe-e))
               (with-issue-info (list (cons 'exn maybe-e)
                                      (cons 'expected expt))
                 (λ [] (spec-collapse))))]
          [(exn:fail? maybe-e)
           (with-issue-info (list (cons 'exn maybe-e)
                                  (cons 'expected (object-name expt)))
             (λ [] (spec-collapse)))]
          [else (spec-collapse)])))

(define-assert (%? [pred : (-> Any Boolean)] [v : Any])
  (with-issue-info (list (cons 'expected (object-name pred))
                         (cons 'given v))
    (λ [] (or (pred v)
              (spec-collapse)))))

(define-assert (%?-regexp [px : (U Byte-Regexp Regexp Bytes String)] [src : (U Path-String Bytes)])
  (with-issue-info (list (cons 'expected (cond [(string? px) (pregexp px)] [(bytes? px) (byte-pregexp px)] [else px]))
                         (cons 'given src))
    (λ [] (or (regexp-match? px src)
              (spec-collapse)))))
