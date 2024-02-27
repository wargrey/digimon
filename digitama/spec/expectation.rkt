#lang typed/racket/base

(provide (except-out (all-defined-out) Spec-Match-Datum $?-logging-routine-values))

(require racket/sequence)

(require "prompt.rkt")
(require "issue.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-spec-expectation stx)
  (syntax-parse stx #:literals [:]
    [(_ (~optional (~seq (~or #:forall #:∀) tvars) #:defaults ([tvars #'()]))
        (id:id [arg:id : Type:expr] ...)
        (~alt (~optional (~seq #:default-format format) #:defaults ([format #'#false]))
              (~optional (~seq #:name alt-name) #:defaults ([alt-name #'#false]))) ...
        body ...)
     (with-syntax ([expect-id (format-id #'id "expect-~a" (syntax-e #'id))]
                   [λexpect-id (format-id #'id "λexpect-~a" (syntax-e #'id))]
                   [check-id (format-id #'id "check-~a" (syntax-e #'id))]
                   [(T ...) (syntax->list #'tvars)])
       (syntax/loc stx
         (begin (define expectation-name : Symbol (or alt-name 'check-id))
                (define expectation-argv : (Listof Symbol) (list 'arg ...))

                (define #:forall (T ...) (λcheck-id [arg : Type] ...) : Void
                  body ... (void))

                (define #:forall (T ...) λexpect-id : (->* (Spec-Syntax Type ...) ((U String Spec-Issue-Format)) #:rest Any Void)
                  (case-lambda
                    [(stx arg ...)
                     (parameterize ([default-spec-issue-expectation expectation-name]
                                    [default-spec-issue-arguments expectation-argv]
                                    [default-spec-issue-parameters (list arg ...)]
                                    [default-spec-issue-expressions (Spec-Syntax-expressions stx)]
                                    [default-spec-issue-locations (cons (Spec-Syntax-location stx) (default-spec-issue-locations))]
                                    [default-spec-issue-message #false]
                                    [default-spec-issue-format (spec-format-stack format (default-spec-issue-format))])
                       (λcheck-id arg ...))]
                    [(stx arg ... usrf . argl)
                     (parameterize ([default-spec-issue-expectation expectation-name]
                                    [default-spec-issue-arguments expectation-argv]
                                    [default-spec-issue-parameters (list arg ...)]
                                    [default-spec-issue-expressions (Spec-Syntax-expressions stx)]
                                    [default-spec-issue-locations (cons (Spec-Syntax-location stx) (default-spec-issue-locations))]
                                    [default-spec-issue-message (if (string? usrf) (spec-message usrf argl) (spec-message argl))]
                                    [default-spec-issue-format (let ([stdf (spec-format-stack format (default-spec-issue-format))])
                                                                 (if (procedure? usrf) (spec-format-stack usrf stdf) stdf))])
                       (λcheck-id arg ...))]))

                (define-syntax (expect-id stx)
                  ; using stx directly causes recursively macro expanding
                  (with-syntax ([loc (datum->syntax stx 'expect-id stx)])
                    (syntax-parse stx
                      [(_:id . arglist) (syntax/loc stx (λexpect-id (Spec-Syntax #'loc #'arglist) #| <= hide them from type-checking error info |# . arglist))]
                      [_:id (syntax/loc stx λexpect-id)]))))))]))

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

(define-spec-boolean-expectation (member [given : Any] [range : (Sequenceof Any)]) (for/or : Boolean ([e range]) (equal? given e)))
(define-spec-boolean-expectation (memv [given : Any] [range : (Sequenceof Any)]) (for/or : Boolean ([e range]) (eqv? given e)))
(define-spec-boolean-expectation (memq [given : Any] [range : (Sequenceof Any)]) (for/or : Boolean ([e range]) (eq? given e)))
(define-spec-boolean-expectation (not-member [given : Any] [range : (Sequenceof Any)]) (for/and : Boolean ([e range]) (not (equal? given e))))
(define-spec-boolean-expectation (not-memv [given : Any] [range : (Sequenceof Any)]) (for/and : Boolean ([e range]) (not (eqv? given e))))
(define-spec-boolean-expectation (not-memq [given : Any] [range : (Sequenceof Any)]) (for/and : Boolean ([e range]) (not (eq? given e))))

(define-spec-boolean-expectation (zero [given : Number]) (zero? given))
(define-spec-boolean-expectation (null [given : Any]) (null? given))
(define-spec-boolean-expectation (void [given : Any]) (void? given))
(define-spec-boolean-expectation (true [given : Any]) (eq? given #true))
(define-spec-boolean-expectation (false [given : Any]) (eq? given #false))
(define-spec-boolean-expectation (not-false [given : Any]) (and given #true))

(define-spec-boolean-expectation (file-exists [given : Path-String]) (file-exists? given))
(define-spec-boolean-expectation (directory-exists [given : Path-String]) (directory-exists? given))

(define-spec-boolean-expectation (= [given : Real] [expected : Real]) (= given expected))
(define-spec-boolean-expectation (!= [given : Real] [expected : Real]) (not (= given expected)))
(define-spec-boolean-expectation (< [given : Real] [origin : Real]) (< given origin))
(define-spec-boolean-expectation (> [given : Real] [origin : Real]) (> given origin))
(define-spec-boolean-expectation (<= [given : Real] [origin : Real]) (<= given origin))
(define-spec-boolean-expectation (>= [given : Real] [origin : Real]) (>= given origin))

(define-spec-boolean-expectation (0b= [given : Integer] [expected : Integer]) #:default-format spec-format/bin (= given expected))
(define-spec-boolean-expectation (0x= [given : Integer] [expected : Integer]) #:default-format spec-format/hex (= given expected))

(define-spec-boolean-expectation (fl= [given : Real] [expected : Real] [epsilon : Nonnegative-Flonum]) (<= (magnitude (- given expected)) epsilon))
(define-spec-boolean-expectation (fl!= [given : Real] [expected : Real] [epsilon : Nonnegative-Flonum]) (> (magnitude (- given expected)) epsilon))

(define-spec-boolean-expectation (octet= [given : Byte] [expected : Byte])    #:default-format spec-format/octet (= given expected))
(define-spec-boolean-expectation (octets= [given : Bytes] [expected : Bytes]) #:default-format spec-format/octet (bytes=? given expected))
(define-spec-boolean-expectation (bytes= [given : Bytes] [expected : Bytes])  #:default-format spec-format/hex (bytes=? given expected))

(define-spec-boolean-expectation (string= [given : String] [expected : String]) (string=? given expected))
(define-spec-boolean-expectation (string-ci= [given : String] [expected : String]) (string-ci=? given expected))

(define-spec-boolean-expectation (char= [given : Char] [expected : Char]) (char=? given expected))
(define-spec-boolean-expectation (char-ci= [given : Char] [expected : Char]) (char-ci=? given expected))

(define-spec-boolean-expectation (eof [given : Any])
  (cond [(input-port? given) (eof-object? (peek-byte given))]
        [else (eof-object? given)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation #:forall (T) (satisfy [predicate : (-> T Boolean)] [given : T])
  (or (predicate given)
      (spec-misbehave)))

(define-spec-expectation #:forall (T) (dissatisfy [predicate : (-> T Boolean)] [given : T])
  (and (predicate given)
       (spec-misbehave)))

(define-spec-expectation #:forall (G E) (is [check : (-> G E Boolean)] [given : G] [expected : E])
  (or (check given expected)
      (spec-misbehave)))

(define-spec-expectation #:forall (G E) (isnt [check : (-> G E Boolean)] [given : G] [expected : E])
  (and (check given expected)
       (spec-misbehave)))

(define-spec-expectation #:forall (G E) (are [check : (-> G E Boolean)] [given : (Sequenceof G)] [expected : (Sequenceof E)])
  (if (= (sequence-length given) (sequence-length expected))
      (for ([g given]
            [e expected]
            #:unless (check g e))
        (spec-misbehave))
      (spec-misbehave)))

(define-spec-expectation #:forall (G E) (arent [check : (-> G E Boolean)] [given : (Sequenceof G)] [expected : (Sequenceof E)])
  (when (= (sequence-length given) (sequence-length expected))
    (for ([g given]
          [e expected]
          #:when (check g e))
      (spec-misbehave))))

(define-spec-expectation #:forall (T) (satisfy-all [predicate : (-> T Boolean)] [givens : (Sequenceof T)])
  (or (sequence-andmap predicate givens)
      (spec-misbehave)))

(define-spec-expectation #:forall (T) (satisfy-any [predicate : (-> T Boolean)] [givens : (Sequenceof T)])
  (or (sequence-ormap predicate givens)
      (spec-misbehave)))

(define-spec-expectation #:forall (T) (satisfy-any+ [predicate : (-> T Boolean)] [givens : (Sequenceof T)] [min : Index])
  (let ([n (sequence-count predicate givens)])
    (unless (< min n (sequence-length givens))
      (spec-misbehave))))

(define-spec-expectation #:forall (T) (satisfy-none [predicate : (-> T Boolean)] [givens : (Sequenceof T)])
  (and (sequence-ormap predicate givens)
       (spec-misbehave)))

(define-spec-expectation #:forall (T) (ordered [givens : (Sequenceof T)] [op : (-> T T Boolean)])
  (when (> (sequence-length givens) 1)
    (for/fold ([r : T (sequence-ref givens 0)])
              ([s (sequence-tail givens 1)])
      (if (op r s) s (spec-misbehave)))))

(define-spec-expectation (match [pattern : Spec-Match-Datum] [given : (U Path-String Bytes Input-Port)])
  (or (cond [(input-port? given) (regexp-match-peek pattern given)]
            [else (regexp-match? pattern given)])
      (spec-misbehave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Match-Datum (U Byte-Regexp Regexp Bytes String))
(define-type Spec-Log-Match-Datum
  (U (-> Any Boolean) Spec-Match-Datum
     (Vectorof (U (-> Any Boolean) True Spec-Match-Datum))
     (Listof (U (-> Any Boolean) Spec-Match-Datum))))

(define $?-logging-routine-values : (Parameterof (Pairof Any (Listof Any))) (make-parameter (list (void))))

(define-spec-expectation (log-message [logsrc : Log-Receiver] [messages : Spec-Log-Match-Datum] [routine : (-> AnyValues)])
  (define-values (/dev/syncin /dev/syncout) (make-pipe))
  
  (define (message-okay? [given : (Immutable-Vector Symbol String Any (Option Symbol))] [expected : (U (-> Any Boolean) Spec-Match-Datum True)]) : Boolean
    (cond [(procedure? expected) (expected (vector-ref given 2))]
          [(boolean? expected) #true]
          [else (regexp-match? expected (vector-ref given 1))]))
  
  (define ghostcat
    (thread
     (λ []
       (define sgol : (Listof (Immutable-Vector Symbol String Any (Option Symbol)))
         (let collect ([sgol0 : (Listof (Immutable-Vector Symbol String Any (Option Symbol))) null])
           (define which (sync logsrc /dev/syncin))
           (cond [(input-port? which)
                  (read-byte /dev/syncin)
                  (let final-prove ([sgol : (Listof (Immutable-Vector Symbol String Any (Option Symbol))) sgol0])
                    (define msg (sync/timeout 0 logsrc))
                    (cond [(vector? msg) (final-prove (cons msg sgol))]
                          [else sgol]))]
                 [else (collect (cons which sgol0))])))
       
       (define okay? : Boolean
         (cond [(list? messages)
                (for/and ([log (in-list sgol)])
                  (for/or : Boolean ([msg (in-list messages)])
                    (message-okay? log msg)))]
               [(vector? messages)
                (and (eq? (length sgol) (vector-length messages))
                     (for/and ([log (in-list (reverse sgol))]
                               [msg (in-vector messages)])
                       (message-okay? log msg)))]
               [else (for/or ([log (in-list sgol)])
                       (message-okay? log messages))]))

       (write-byte (if okay? 1 0) /dev/syncout))))
  
  (call-with-values routine
    (λ results
      (cond [(pair? results) ($?-logging-routine-values results)]
            [else #| deadcode |# (spec-misbehave)])))
  
  (write-byte 0 /dev/syncout)
  (thread-wait ghostcat)
  (or (eq? (read-byte /dev/syncin) 1)
      (spec-misbehave)))

(define-spec-expectation (log-message* [logsrc : Log-Receiver] [messages : Spec-Log-Match-Datum] [routine : (-> AnyValues)]
                                       [check-routine : (U (-> (Pairof Any (Listof Any)) AnyValues) (Boxof (Pairof Any (Listof Any))))])
  (expect-log-message logsrc messages routine)
  
  (if (box? check-routine)
      (set-box! check-routine ($?-logging-routine-values))
      (check-routine ($?-logging-routine-values))))

(define-spec-expectation (throw [exception : (U (-> Any Boolean) Spec-Match-Datum)] [routine : (-> Any)])
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

(define-spec-expectation (noexcept [routine : (-> Any)])
  (define maybe-e : Any (with-handlers ([exn:fail? values]) (void (routine))))

  (when (exn:fail? maybe-e)
    (parameterize ([default-spec-issue-exception maybe-e])
      (spec-misbehave))))
