#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/symbol)

(require "prompt.rkt")
(require "issue.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-spec-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (id:id [arg:id : Type:expr] ...)
        (~alt (~optional (~seq #:default-format format) #:defaults ([format #'#false]))
              (~optional (~seq #:name alt-name) #:defaults ([alt-name #'#false]))) ...
        body ...)
     (with-syntax ([expect-id (format-id #'id "expect-~a" (syntax-e #'id))])
       (syntax/loc stx
         (begin (define argv : (Listof String) (list (symbol->immutable-string 'arg) ...))
                (define argc : Index (length argv))
                
                (define make-expect : (-> Symbol Syntax (Listof Any) (->* (Type ...) (String) #:rest Any Void))
                  (lambda [name loc exprs]
                    (define (expect-id [arg : Type] ... . [argl : Any *]) : Void
                      (parameterize ([default-spec-issue-expectation (or alt-name name)]
                                     [default-spec-issue-message (spec-message argl)]
                                     [default-spec-issue-location (or (default-spec-issue-location) (spec-location loc))]
                                     [default-spec-issue-expressions exprs]
                                     [default-spec-issue-arguments argv]
                                     [default-spec-issue-parameters (list arg ...)]
                                     [default-spec-issue-format (or (default-spec-issue-format) format)])
                        body ... (void)))
                    expect-id))

                (define-syntax (expect-id stx)
                  (with-syntax ([loc (datum->syntax #false "use stx directly causes recursively macro expanding" stx)])
                    (syntax-parse stx
                      [(_:id . arglist) (syntax/loc stx ((make-expect 'id #'loc (take 'arglist argc)) . arglist))]
                      [_:id (syntax/loc stx (make-expect 'id #'loc 'expect-id))]))))))]))

(define-syntax (define-spec-boolean-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (?:id [arg:id : Type:expr] ...)
        (~alt (~optional (~seq #:default-format format) #:defaults ([format #'#false]))
              (~optional (~seq #:name alt-name) #:defaults ([alt-name #'#false]))) ...
        body ...)
     (syntax/loc stx
       (define-spec-expectation (? [arg : Type] ...)
         #:default-format format
         #:name 'alt-name
         (or (let () (void) body ...)
             (spec-misbehave))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-boolean-expectation (eq [given : Any] [expected : Any]) (eq? expected given))
(define-spec-boolean-expectation (eqv [given : Any] [expected : Any]) (eqv? expected given))
(define-spec-boolean-expectation (equal [given : Any] [expected : Any]) (equal? expected given))
(define-spec-boolean-expectation (not-eq [given : Any] [origin : Any]) (not (eq? given origin)))
(define-spec-boolean-expectation (not-eqv [given : Any] [origin : Any]) (not (eqv? given origin)))
(define-spec-boolean-expectation (not-equal [given : Any] [origin : Any]) (not (equal? given origin)))

(define-spec-boolean-expectation (member [given : Any] [range : (Listof Any)]) (member given range))
(define-spec-boolean-expectation (memv [given : Any] [range : (Listof Any)]) (memv given range))
(define-spec-boolean-expectation (memq [given : Any] [range : (Listof Any)]) (memq given range))
(define-spec-boolean-expectation (not-member [given : Any] [range : (Listof Any)]) (not (member given range)))
(define-spec-boolean-expectation (not-memv [given : Any] [range : (Listof Any)]) (not (memv given range)))
(define-spec-boolean-expectation (not-memq [given : Any] [range : (Listof Any)]) (not (memq given range)))

(define-spec-boolean-expectation (zero [given : Number]) (zero? given))
(define-spec-boolean-expectation (null [given : Any]) (null? given))
(define-spec-boolean-expectation (void [given : Any]) (void? given))
(define-spec-boolean-expectation (true [given : Any]) (eq? given #true))
(define-spec-boolean-expectation (false [given : Any]) (eq? given #false))
(define-spec-boolean-expectation (not-false [given : Any]) (and given #true))

(define-spec-boolean-expectation (= [given : Real] [expected : Real]) (= given expected))
(define-spec-boolean-expectation (< [given : Real] [origin : Real]) (< given origin))
(define-spec-boolean-expectation (> [given : Real] [origin : Real]) (> given origin))
(define-spec-boolean-expectation (<= [given : Real] [origin : Real]) (<= given origin))
(define-spec-boolean-expectation (>= [given : Real] [origin : Real]) (>= given origin))

(define-spec-boolean-expectation (0b= [given : Integer] [expected : Integer]) #:default-format spec-format/bin (= given expected))
(define-spec-boolean-expectation (0x= [given : Integer] [expected : Integer]) #:default-format spec-format/hex (= given expected))

(define-spec-boolean-expectation (fl= [given : Flonum] [expected : Flonum] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- given expected)) epsilon))

(define-spec-boolean-expectation (octet= [given : Byte] [expected : Byte])       #:default-format spec-format/octet (= given expected))
(define-spec-boolean-expectation (octets= [given : Bytes] [expected : Bytes])    #:default-format spec-format/octet (bytes=? given expected))
(define-spec-boolean-expectation (bytes= [given : Bytes] [expected : Bytes])     #:default-format spec-format/hex (bytes=? given expected))
(define-spec-boolean-expectation (bytes-ci= [given : Bytes] [expected : Bytes])  #:default-format spec-format/hex (bytes=? given expected))

(define-spec-boolean-expectation (string= [given : String] [expected : String]) (string=? given expected))
(define-spec-boolean-expectation (string-ci= [given : String] [expected : String]) (string-ci=? given expected))

(define-spec-boolean-expectation (eof [given : Any])
  (cond [(input-port? given) (eof-object? (peek-byte given))]
        [else (eof-object? given)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation (throw [exception : (U (-> Any Boolean) (U Byte-Regexp Regexp Bytes String))] [routine : (-> Any)])
  (define maybe-e (with-handlers ([exn:fail? values]) (void (routine))))
  
  (let ([e? (if (procedure? exception) exception exn:fail?)])
    (cond [(and (exn:fail? maybe-e) (e? maybe-e))
           (or (procedure? exception)
               (regexp-match? exception (exn-message maybe-e))
               (parameterize ([default-spec-issue-exception maybe-e])
                 (spec-misbehave)))]
          [(exn:fail? maybe-e)
           (parameterize ([default-spec-issue-exception maybe-e])
             (spec-misbehave))]
          [else (spec-misbehave)])))

(define-spec-expectation (no-exception [routine : (-> Any)])
  (define maybe-e : Any (with-handlers ([exn:fail? values]) (void (routine))))

  (when (exn:fail? maybe-e)
    (parameterize ([default-spec-issue-exception maybe-e])
      (spec-misbehave))))

(define-spec-expectation (satisfy [predicate : (-> Any Boolean)] [given : Any])
  (or (predicate given)
      (spec-misbehave)))

(define-spec-expectation (regexp-match [px : (U Byte-Regexp Regexp Bytes String)] [given : (U Path-String Bytes Input-Port)])
  (or (cond [(input-port? given) (regexp-match-peek px given)]
            [else (regexp-match? px given)])
      (spec-misbehave)))
