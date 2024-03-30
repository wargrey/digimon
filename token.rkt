#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [port-next-location syn-token-port-location]))

(require racket/path)

(require typed/racket/unsafe)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(unsafe-require/typed
 racket/base ; the line is gauranteed to count, hence the explicitly requiring.
 [port-next-location (-> Port (Values Positive-Integer Natural Positive-Integer))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Syn-Token-Stdin (U Input-Port Path-String Bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-token-interface stx)
  (syntax-case stx [:]
    [(_ symbolic-id : Type id? id-datum #:+ SubToken #:eq? type=? #:for Syntax-Any #:throw exn:range)
     (with-syntax ([<id> (format-id #'symbolic-id "<~a>" (syntax-e #'symbolic-id))]
                   [id=<-? (format-id #'symbolic-id "~a=<-?" (syntax-e #'symbolic-id))]
                   [id=:=? (format-id #'symbolic-id "~a=:=?" (syntax-e #'symbolic-id))]
                   [make-exn:range (format-id #'exn:range "make-~a" (syntax-e #'exn:range))])
       (syntax/loc stx
         (begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ SubToken]
                                                  [Any (U (-> Type Boolean) (Listof Type)) -> (Option Type) : #:+ SubToken]))
                  (lambda [token range?]
                    (and (id? token)
                         (let ([datum : Type (id-datum token)])
                           (cond [(procedure? range?) (and (range? datum) datum)]
                                 [else (and (member datum range? type=?) datum)])))))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (-> Syntax-Any (U exn:range False a))]
                                                [(U (-> Type Boolean) (Listof Type) Type) -> (-> Syntax-Any (U exn:range False Type))]
                                                [-> (-> Syntax-Any (U exn:range False Type))]))
                  (case-lambda
                    [() (λ [[t : Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(range?) (cond [(procedure? range?)
                                     (λ [[t : Syntax-Any]]
                                       (and (id? t)
                                            (or (let ([d : Type (id-datum t)]) (and (range? d) d))
                                                (make-exn:range t))))]
                                    [(list? range?)
                                     (λ [[t : Syntax-Any]]
                                       (and (id? t)
                                            (let ([d : Type (id-datum t)])
                                              (cond [(member d range? type=?) d]
                                                    [else (make-exn:range t)]))))]
                                    [else (λ [[t : Syntax-Any]]
                                            (and (id? t)
                                                 (let ([d : Type (id-datum t)])
                                                   (if (type=? d range?) d (make-exn:range t)))))])]))

                (define id=:=? : (-> Any Type (Option Type) : #:+ SubToken) #| for performance |#
                  (lambda [t v]
                    (and (id? t)
                         (let ([d : Type (id-datum t)])
                           (and (type=? d v) d))))))))]
    
    [(_ numeric-id : Type id? id-datum #:+ SubToken #:= type=? #:for Syntax-Any #:throw exn:range)
     (with-syntax ([<id> (format-id #'numeric-id "<~a>" (syntax-e #'numeric-id))]
                   [id=<-? (format-id #'numeric-id "~a=<-?" (syntax-e #'numeric-id))]
                   [make-exn:range (format-id #'exn:range "make-~a" (syntax-e #'exn:range))])
       (syntax/loc stx
         (begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ SubToken]
                                                  [Any (-> Type Type Boolean) Type -> (Option Type) : #:+ SubToken]
                                                  [Any Type (-> Type Type Boolean) Type -> (Option Type) : #:+ SubToken]
                                                  [Any (Listof Type) -> (Option Type) : #:+ SubToken]))
                  (case-lambda
                    [(token op n)   (and (id? token) (let ([d : Type (id-datum token)]) (and (op d n) d)))]
                    [(token l op r) (and (id? token) (let ([m : Type (id-datum token)]) (and (op l m) (op m r) m)))]
                    [(token range?) (and (id? token) (let ([d : Type (id-datum token)])
                                                       (cond [(procedure? range?) (and (range? d) d)]
                                                             [else (for/or : (Option Type) ([v (in-list range?)])
                                                                     (and (type=? d v) d))])))]))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (-> Syntax-Any (U exn:range False a))]
                                                [(-> Type Type Boolean) Type -> (-> Syntax-Any (U exn:range False Type))]
                                                [Type (-> Type Type Boolean) Type -> (-> Syntax-Any (U exn:range False Type))]
                                                [(Listof Type) -> (-> Syntax-Any (U exn:range False Type))]
                                                [-> (-> Syntax-Any (U exn:range False Type))]))
                  (case-lambda
                    [() (λ [[t : Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(op n) (λ [[t : Syntax-Any]]
                              (and (id? t)
                                   (let ([d : Type (id-datum t)])
                                     (if (op d n) d (make-exn:range t)))))]
                    [(l op r) (λ [[t : Syntax-Any]]
                                (and (id? t)
                                     (let ([m : Type (id-datum t)])
                                       (if (and (op l m) (op m r)) m (make-exn:range t)))))]
                    [(range?) (λ [[t : Syntax-Any]]
                                (and (id? t)
                                     (let ([d : Type (id-datum t)])
                                       (or (cond [(procedure? range?) (and (range? d) d)]
                                                 [(list? range?) (and (member d range? type=?) d)]
                                                 [else (and (type=? d range?) d)])
                                           (make-exn:range t)))))])))))]))

(define-syntax (define-syntax-error stx)
  (syntax-case stx []
    [(_ exn:syn #:as Syntax-Error #:for Token #:with [make-syntax-error log-syntax-error]
        [subexn #:-> parent] ...)
     (with-syntax ([([make-exn make+exn throw-exn] ...)
                    (for/list ([<exn> (in-list (syntax->list #'(subexn ...)))])
                      (list (format-id <exn> "make-~a" (syntax-e <exn>))
                            (format-id <exn> "make+~a" (syntax-e <exn>))
                            (format-id <exn> "throw-~a" (syntax-e <exn>))))])
       (syntax/loc stx
         (begin (define-type Syntax-Error exn:syn)
                (struct exn:syn exn:fail:syntax ())
                (struct subexn parent ()) ...

                (define make-exn : (-> (U False Token (Listof Token)) subexn)
                  (lambda [v]
                    (make-syntax-error subexn v)))
                ...

                (define make+exn : (->* ((U False Token (Listof Token))) ((Option Token) (Option Log-Level)) subexn)
                  (lambda [v [property #false] [level #false]]
                    (define errobj : subexn (make-syntax-error subexn v))
                    (log-syntax-error errobj property level)
                    errobj))
                ...

                (define throw-exn : (->* ((U False Token (Listof Token))) ((Option Token) Log-Level) Nothing)
                  (lambda [v [property #false] [level 'warning]]
                    (raise (make+exn v property level))))
                ...)))]))

(define-syntax (syn-remake-token stx)
  (syntax-case stx []
    [(_ [start-token end-token] make-syn:token datum extra ...)
     (syntax/loc stx
       (make-syn:token (syn-token-source start-token) (syn-token-line start-token)
                       (syn-token-column start-token) (syn-token-start start-token)
                       (syn-token-end end-token) datum extra ...))]
    [(_ here-token make-syn:token datum ...)
     (syntax/loc stx (syn-remake-token [here-token here-token] make-syn:token datum ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct syn-token
  ([source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer] ; `start` and `end` (instead of `position` and `span`) are required by color lexer.
   [end : Positive-Integer])
  #:type-name SYN-Token
  #:transparent)

(define syn-token->syntax-location : (-> SYN-Token (Vector Any (Option Integer) (Option Integer) (Option Integer) (Option Integer)))
  (lambda [instance]
    (vector (syn-token-source instance) (syn-token-line instance) (syn-token-column instance)
            (syn-token-start instance) (- (syn-token-end instance) (syn-token-start instance)))))

(define syn-token-location-string : (-> SYN-Token String)
  (lambda [instance]
    (format "~a:~a:~a" (syn-token-source instance) (syn-token-line instance) (add1 (syn-token-column instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define syn-log-exn : (->* ((U exn String) Symbol) (Any Log-Level) Void)
  (lambda [errobj topic [src #false] [level 'debug]]
    (define message : String
      (cond [(string? errobj) errobj]
            [else (format "@~s: ~a: ~a" src
                    (object-name errobj) (exn-message errobj))]))
    
    (log-message (current-logger) level topic message errobj)))

(define #:forall (T Error) syn-token->exn
  : (case-> [(-> String Continuation-Mark-Set (Listof Syntax) Error) (->* (T) ((Option Any) (Option Any)) String) (-> T Syntax) T -> Error]
            [(-> String Continuation-Mark-Set (Listof Syntax) Error) (->* (T) ((Option Any) (Option Any)) String) (-> T Syntax) (-> T String) T (Listof T) -> Error])
  (let ([syn-empty-stack (continuation-marks #false)])
    (case-lambda
      [(exn:syn token->string token->syntax main)
       (exn:syn (token->string main exn:syn) syn-empty-stack (list (token->syntax main)))]
      [(exn:syn token->string token->syntax token-datum->string head others)
       (exn:syn (format "~a ~a" (token->string head exn:syn) (map token-datum->string others))
                syn-empty-stack (map token->syntax (cons head others)))])))

(define #:forall (T Error) syn-log-syntax-error : (->* (Symbol (->* (T) ((Option Any) (Option Any)) String) (-> T Any) Error) ((Option T) Log-Level) Void)
  (lambda [topic token->string token->datum errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define msg : String (exn-message (assert errobj exn?)))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(not property) (log-message logger level topic msg errobj)]
          [(syn-token? property)
           (let ([property-msg (format "<~a:~a:~a>" (token->datum property) (syn-token-line property) (add1 (syn-token-column property)))])
             (cond [(not <eof>?) (log-message logger level topic (format "~a @~a" msg property-msg) errobj)]
                   [else (let ([eof-msg (token->string property errobj eof)])
                           (log-message logger level topic (format "~a @~a" eof-msg property-msg) errobj))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define syn-token-stdin->port : (->* (Syn-Token-Stdin Regexp Any) ((U String Symbol False)) Input-Port)
  (lambda [/dev/stdin rx.ext basename [port-name #false]]
    (cond [(port? /dev/stdin) /dev/stdin]
          [(path? /dev/stdin) (open-input-file /dev/stdin)]
          [(regexp-match? rx.ext /dev/stdin) (open-input-file (format "~a" /dev/stdin))]
          [(string? /dev/stdin) (open-input-string /dev/stdin (or port-name (format "/dev/~a/string" basename)))]
          [(bytes? /dev/stdin) (open-input-bytes /dev/stdin (or port-name (format "/dev/~a/bytes" basename)))]
          [else (open-input-string (format "~s" /dev/stdin) (or port-name (format "/dev/~a/error" basename)))])))

(define syn-token-port-name : (-> Input-Port (U String Symbol))
  (lambda [/dev/stdin]
    (define src (object-name /dev/stdin))

    (cond [(path? src) (path->string (simple-form-path src))]
          [(symbol? src) src]
          [(string? src) (string->symbol src)]
          [else (string->symbol (format "~a" src))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define syn-token-skip-shebang-line : (-> Input-Port Void)
  (lambda [/dev/stdin]
    (when (regexp-try-match #px"^\\s*[#][!]" /dev/stdin)
      (read-line /dev/stdin)
      (void))))

(define syn-token-skip-whitespace : (-> Input-Port Void)
  (lambda [/dev/stdin]
    (regexp-match-positions #px"^\\s*" /dev/stdin)
    (void)))

(define syn-token-port-skip-lang-line : (-> Input-Port Void)
  (lambda [/dev/stdin]
    (let skip ()
      (define ch : (U EOF Char) (peek-char /dev/stdin))

      (unless (eof-object? ch)
        (cond [(eq? ch #\#)
               (if (equal? (peek-bytes 5 1 /dev/stdin) #"lang ")
                   (read-line /dev/stdin 'any)
                   (let ([nch (peek-byte /dev/stdin 1)])
                     (cond [(eq? nch #;#\; #x3B) (read /dev/stdin) (skip)]
                           [(eq? nch #;#\| #x7C) (regexp-match-positions #px"^.*(?<=[|])[#]\\s*" /dev/stdin) (skip)])))]
              [(eq? ch #\;) (read-line /dev/stdin 'any) (skip)]
              [(char-whitespace? ch) (regexp-match-positions #px"^\\s+" /dev/stdin) (skip)])))
      
    (syn-token-skip-whitespace /dev/stdin)))

(define syn-token-peek-char : (-> Input-Port (U EOF Char))
  (lambda [/dev/stdin]
    (syn-token-skip-whitespace /dev/stdin)
    (peek-char /dev/stdin)))

(define syn-token-read-char : (-> Input-Port (U EOF Char))
  (lambda [/dev/stdin]
    (syn-token-skip-whitespace /dev/stdin)
    (read-char /dev/stdin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define syn-token-fallback-charset : (-> Bytes String)
  (lambda [from]
    (define CHARSET : String (string-upcase (bytes->string/utf-8 from)))
    (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
          [else CHARSET])))
