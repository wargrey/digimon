#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "prompt.rkt")
(require "issue.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-spec-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (id:id [arg:id : Type:expr] ...) body ...)
     (with-syntax ([expect-id (format-id #'id "expect-~a" (syntax-e #'id))]
                   [behave-id (format-id #'id "behave-~a" (syntax-e #'id))])
       #'(begin (define do-expecting : (-> Symbol Syntax Any (->* (Type ...) (String) #:rest Any Void))
                  (lambda [name stx expr]
                    (define (expect-id [arg : Type] ... . [argl : Any *]) : Void
                      (define message : (Option String)
                        (and (pair? argl) (string? (car argl))
                             (cond [(null? (cdr argl)) (car argl)]
                                   [else (apply format (car argl) (cdr argl))])))
                      ((inst with-issue-info Void)
                       (list (cons 'name name)
                             (cons 'location (issue-location stx))
                             (cons 'expression (cons name expr))
                             (cons 'arguments (list arg ...))
                             (cons 'message message))
                       (λ [] body ... (void))))
                    expect-id))

                (define do-behaving : (-> Symbol Syntax Any (-> Type ... String Any * (U Void Spec-Issue)))
                  (lambda [name stx expr]
                    (define ? : (->* (Type ...) (String) #:rest Any Void) (do-expecting name stx expr))
                    (define (behave-id [arg : Type] ... [brfmt : String] . [argl : Any *]) : (U Void Spec-Issue)
                      (define brief : String
                        (cond [(null? argl) brfmt] 
                              [else (apply format brfmt argl)]))
                      (spec-case brief (? arg ...)))
                    behave-id))
                
                (define argc : Index (length '(arg ...)))
                
                (define-syntax (expect-id stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_:id . arglist) #'((do-expecting 'id #'loc (take 'arglist argc)) . arglist)]
                      [_:id #'(do-expecting 'id #'loc 'expect-id)])))

                (define-syntax (behave-id stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_:id . arglist) #'((do-behaving 'id #| not a typo |# #'loc (take 'arglist argc)) . arglist)]
                      [_:id #'(do-behaving 'id #| not a typo |# #'loc 'behave-id)])))))]))

(define-syntax (define-spec-boolean-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...) body ...)
     #'(define-spec-expectation (? [arg : Type] ...)
         (or (let () (void) body ...)
             (spec-misbehave)))]))

(define-syntax (define-spec-binary-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [gv:id : GType:expr] [ev:id : EType:expr]) body ...)
     #'(define-spec-expectation (? [gv : GType] [ev : EType])
         (with-issue-info (list (cons 'expected ev)
                                (cons 'given gv))
           (λ [] (or (let () (void) body ...)
                     (spec-misbehave)))))]
    [(_ (?:id pred:id [gv:id : GType:expr] [ev:id : EType:expr]))
     #'(define-spec-binary-expectation (? [gv : GType] [ev : EType])
         (pred gv ev))]
    [(_ (?:id [pred:id : (-> GType:expr EType:expr Type)]))
     #'(define-spec-binary-expectation (? [gv : GType] [ev : EType])
         (pred gv ev))]))

(define-syntax (spec-begin stx)
  (syntax-parse stx #:literals [:]
    [(_ asserts ...)
     #'((inst spec-story Spec-Issue)
        (gensym 'spec)
        (λ [] (void asserts ...))
        values)]))

(define-syntax (spec-case stx)
  (syntax-parse stx #:literals [:]
    [(_ [brief:str] asserts ...)
     #'(spec-case brief asserts ...)]
    [(_ [brfmt:str argl ...] asserts ...)
     #'(let ([brief (format brfmt argl ...)])
         (spec-case brief asserts ...))]
    [(_ brief asserts ...)
     #'(parameterize ([default-spec-issue-brief brief])
         (spec-begin asserts ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-binary-expectation (eq [v1 : Any] [v2 : Any]) (eq? v1 v2))
(define-spec-binary-expectation (eqv [v1 : Any] [v2 : Any]) (eqv? v1 v2))
(define-spec-binary-expectation (equal [v1 : Any] [v2 : Any]) (equal? v1 v2))
(define-spec-boolean-expectation (not-eq [v1 : Any] [v2 : Any]) (not (eq? v1 v2)))
(define-spec-boolean-expectation (not-eqv [v1 : Any] [v2 : Any]) (not (eqv? v1 v2)))
(define-spec-boolean-expectation (not-equal [v1 : Any] [v2 : Any]) (not (equal? v1 v2)))

(define-spec-boolean-expectation (true [v : Any]) (eq? v #true))
(define-spec-boolean-expectation (false [v : Any]) (eq? v #false))
(define-spec-boolean-expectation (not-false [v : Any]) v)
(define-spec-boolean-expectation (collapse) #false)

(define-spec-boolean-expectation (fl= [v1 : Flonum] [v2 : Flonum] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- v1 v2)) epsilon))

(define-spec-expectation (throw [expt : (U (-> Any Boolean) (U Byte-Regexp Regexp Bytes String))] [do-task : (-> Any)])
  (define maybe-e (with-handlers ([exn:fail? values]) (void (do-task))))
  (let ([e? (if (procedure? expt) expt exn:fail?)])
    (cond [(and (exn:fail? maybe-e) (e? maybe-e))
           (or (procedure? expt)
               (regexp-match? expt (exn-message maybe-e))
               (with-issue-info (list (cons 'exn maybe-e)
                                      (cons 'expected expt))
                 (λ [] (spec-misbehave))))]
          [(exn:fail? maybe-e)
           (with-issue-info (list (cons 'exn maybe-e)
                                  (cons 'expected (object-name expt)))
             (λ [] (spec-misbehave)))]
          [else (spec-misbehave)])))

(define-spec-expectation (no-exception [do-task : (-> Any)])
  (define maybe-exn : Any (with-handlers ([exn:fail? values]) (void (do-task))))

  (when (exn? maybe-exn)
    (with-issue-info (list (cons 'exn maybe-exn))
      (λ [] (spec-misbehave)))))

(define-spec-expectation (satisfy [pred : (-> Any Boolean)] [v : Any])
  (with-issue-info (list (cons 'expected (object-name pred))
                         (cons 'given v))
    (λ [] (or (pred v)
              (spec-misbehave)))))

(define-spec-expectation (regexp-match [px : (U Byte-Regexp Regexp Bytes String)] [src : (U Path-String Bytes)])
  (with-issue-info (list (cons 'expected (cond [(string? px) (pregexp px)] [(bytes? px) (byte-pregexp px)] [else px]))
                         (cons 'given src))
    (λ [] (or (regexp-match? px src)
              (spec-misbehave)))))
