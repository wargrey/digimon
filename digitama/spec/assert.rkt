#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "prompt.rkt")
(require "issue.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-spec-primitive stx)
  (syntax-parse stx #:literals [:]
    [(_ (id:id [arg:id : Type:expr] ...) body ...)
     (with-syntax ([%? (format-id #'id "%?-~a" (syntax-e #'id))]
                   [%t (format-id #'id "%test-~a" (syntax-e #'id))])
       #'(begin (define do-assert : (-> Symbol Syntax Any (->* (Type ...) (String) #:rest Any Void))
                  (lambda [name stx expr]
                    (define (id [arg : Type] ... . [argl : Any *]) : Void
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
                    id))

                (define do-test : (-> Symbol Syntax Any (-> Type ... String Any * (U Void Spec-Issue)))
                  (lambda [name stx expr]
                    (define ? : (->* (Type ...) (String) #:rest Any Void) (do-assert name stx expr))
                    (define (%t [arg : Type] ... [brfmt : String] . [argl : Any *]) : (U Void Spec-Issue)
                      (define brief : String
                        (cond [(null? argl) brfmt] 
                              [else (apply format brfmt argl)]))
                      (spec-case brief (? arg ...)))
                    %t))
                
                (define argc : Index (length '(arg ...)))
                
                (define-syntax (%? stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_ . arglist) #'((do-assert '%? #'loc (take 'arglist argc)) . arglist)]
                      [_ #'(do-assert '%? #'loc '%?)])))

                (define-syntax (%t stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_ brief . arglist) #'((do-test '%t #'loc 'arglist) brief . arglist)]
                      [_ #'(do-test '%t #'loc '%t)])))))]))

(define-syntax (define-spec-boolean-primitive stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...) body ...)
     #'(define-spec-primitive (? [arg : Type] ...)
         (or (let () (void) body ...)
             (spec-collapse)))]))

(define-syntax (define-spec-binary-primitive stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [gv:id : GType:expr] [ev:id : EType:expr]) body ...)
     #'(define-spec-primitive (? [gv : GType] [ev : EType])
         (with-issue-info (list (cons 'expected ev)
                                (cons 'given gv))
           (λ [] (or (let () (void) body ...)
                     (spec-collapse)))))]
    [(_ (?:id pred:id [gv:id : GType:expr] [ev:id : EType:expr]))
     #'(define-spec-binary-primitive (? [gv : GType] [ev : EType])
         (pred gv ev))]
    [(_ (?:id [pred:id : (-> GType:expr EType:expr Type)]))
     #'(define-spec-binary-primitive (? [gv : GType] [ev : EType])
         (pred gv ev))]))

(define-syntax (spec-begin stx)
  (syntax-parse stx #:literals [:]
    [(_ asserts ...)
     #'((inst spec-prompt Spec-Issue)
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
(define-spec-binary-primitive (eq [v1 : Any] [v2 : Any]) (eq? v1 v2))
(define-spec-binary-primitive (eqv [v1 : Any] [v2 : Any]) (eqv? v1 v2))
(define-spec-binary-primitive (equal [v1 : Any] [v2 : Any]) (equal? v1 v2))
(define-spec-boolean-primitive (!eq [v1 : Any] [v2 : Any]) (not (eq? v1 v2)))
(define-spec-boolean-primitive (!eqv [v1 : Any] [v2 : Any]) (not (eqv? v1 v2)))
(define-spec-boolean-primitive (!equal [v1 : Any] [v2 : Any]) (not (equal? v1 v2)))

(define-spec-boolean-primitive (true [v : Any]) (eq? v #true))
(define-spec-boolean-primitive (false [v : Any]) (eq? v #false))
(define-spec-boolean-primitive (!false [v : Any]) v)
(define-spec-boolean-primitive ($) #false)

(define-spec-boolean-primitive (fl= [v1 : Flonum] [v2 : Flonum] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- v1 v2)) epsilon))

(define-spec-primitive (exn [expt : (U (-> Any Boolean) (U Byte-Regexp Regexp Bytes String))] [do-task : (-> Any)])
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

(define-spec-primitive (!exn [do-task : (-> Any)])
  (define maybe-exn : Any (with-handlers ([exn:fail? values]) (void (do-task))))

  (when (exn? maybe-exn)
    (with-issue-info (list (cons 'exn maybe-exn))
      (λ [] (spec-collapse)))))

(define-spec-primitive (|| [pred : (-> Any Boolean)] [v : Any])
  (with-issue-info (list (cons 'expected (object-name pred))
                         (cons 'given v))
    (λ [] (or (pred v)
              (spec-collapse)))))

(define-spec-primitive (regexp [px : (U Byte-Regexp Regexp Bytes String)] [src : (U Path-String Bytes)])
  (with-issue-info (list (cons 'expected (cond [(string? px) (pregexp px)] [(bytes? px) (byte-pregexp px)] [else px]))
                         (cons 'given src))
    (λ [] (or (regexp-match? px src)
              (spec-collapse)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define do-test : (-> (->* ((-> Any Boolean) Any) (String) #:rest Any Void) (-> (-> Any Boolean) Any String Any * (U Spec-Issue Void)) (-> Any Boolean) Any Any)
  (lambda [%? %t f a]
    (list (spec-begin
           (%? f a "inside"))
          (%t f a "test"))))

(spec-begin
 (%?- flonum? 1 "jajaja"))

(%test- flonum? 1 "flonum")

(do-test %?- %test- flonum? 1)
