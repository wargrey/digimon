#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "prompt.rkt")
(require "issue.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-spec-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (id:id [arg:id : Type:expr] ...) body ...)
     (with-syntax ([expect-id (format-id #'id "expect-~a" (syntax-e #'id))])
       #'(begin (define do-expecting : (-> Symbol Syntax (Listof Any) (->* (Type ...) (String) #:rest Any Void))
                  (lambda [name stx exprs]
                    (define (expect-id [arg : Type] ... . [argl : Any *]) : Void
                      (parameterize ([default-spec-issue-expectation name]
                                     [default-spec-issue-message (spec-message argl)]
                                     [default-spec-issue-location (spec-location stx)]
                                     [default-spec-issue-expressions exprs]
                                     [default-spec-issue-arguments (list (cons 'arg arg) ...)])
                        body ... (void)))
                    expect-id))

                (define argc : Index (length '(arg ...)))
                
                (define-syntax (expect-id stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_:id . arglist) #'((do-expecting 'id #'loc (take 'arglist argc)) . arglist)]
                      [_:id #'(do-expecting 'id #'loc 'expect-id)])))))]))

(define-syntax (define-spec-boolean-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...) body ...)
     #'(define-spec-expectation (? [arg : Type] ...)
         (or (let () (void) body ...)
             (spec-misbehave)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-boolean-expectation (eq [given : Any] [expected : Any]) (eq? expected given))
(define-spec-boolean-expectation (eqv [given : Any] [expected : Any]) (eqv? expected given))
(define-spec-boolean-expectation (equal [given : Any] [expected : Any]) (equal? expected given))
(define-spec-boolean-expectation (not-eq [v1 : Any] [v2 : Any]) (not (eq? v1 v2)))
(define-spec-boolean-expectation (not-eqv [v1 : Any] [v2 : Any]) (not (eqv? v1 v2)))
(define-spec-boolean-expectation (not-equal [v1 : Any] [v2 : Any]) (not (equal? v1 v2)))

(define-spec-boolean-expectation (true [given : Any]) (eq? given #true))
(define-spec-boolean-expectation (false [given : Any]) (eq? given #false))
(define-spec-boolean-expectation (not-false [given : Any]) given)
(define-spec-boolean-expectation (collapse) #false)

(define-spec-boolean-expectation (fl= [v1 : Flonum] [v2 : Flonum] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- v1 v2)) epsilon))

(define-spec-expectation (throw [except : (U (-> Any Boolean) (U Byte-Regexp Regexp Bytes String))] [do-task : (-> Any)])
  (define maybe-e (with-handlers ([exn:fail? values]) (void (do-task))))
  (let ([e? (if (procedure? except) except exn:fail?)])
    (cond [(and (exn:fail? maybe-e) (e? maybe-e))
           (or (procedure? except)
               (regexp-match? except (exn-message maybe-e))
               (parameterize ([default-spec-issue-exception maybe-e])
                 (spec-misbehave)))]
          [(exn:fail? maybe-e)
           (parameterize ([default-spec-issue-exception maybe-e])
             (spec-misbehave))]
          [else (spec-misbehave)])))

(define-spec-expectation (no-exception [do-task : (-> Any)])
  (define maybe-e : Any (with-handlers ([exn:fail? values]) (void (do-task))))

  (when (exn:fail? maybe-e)
    (parameterize ([default-spec-issue-exception maybe-e])
      (spec-misbehave))))

(define-spec-expectation (satisfy [predicate : (-> Any Boolean)] [given : Any])
  (or (predicate given)
      (spec-misbehave)))

(define-spec-expectation (regexp-match [px : (U Byte-Regexp Regexp Bytes String)] [given : (U Path-String Bytes)])
  (or (regexp-match? px given)
      (spec-misbehave)))
