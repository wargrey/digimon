#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)
(require racket/symbol)
(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Throw-Range-Error (-> (Option Input-Port) (U Symbol Procedure) (U Procedure String (Pairof Real Real) (Listof Any)) Any Any * Nothing))
(define-type Throw-Range-Error* (-> (U Symbol Procedure) (U Procedure String (Pairof Real Real) (Listof Any)) Any Any * Nothing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct exn:fail:read:signature exn:fail:read () #:extra-constructor-name make-exn:read:signature)
(struct exn:fail:read:unsupported exn:fail:read () #:extra-constructor-name make-exn:read:unsupported)
(struct exn:fail:syntax:check exn:fail:syntax () #:extra-constructor-name make-exn:syntax:check)
(struct exn:fail:syntax:range exn:fail:syntax () #:extra-constructor-name make-exn:syntax:range)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define throw-impossible-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read:eof (format "~a: ~a" (port-name /dev/stdin)
                                     (exn-src+args->message src (if (null? args) (list "[I have a bug report]!") args)))
                                   (current-continuation-marks)
                                   null))))

(define throw-eof-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read:eof (format "~a: ~a" (port-name /dev/stdin)
                                     (exn-src+args->message src (cond [(pair? args) args]
                                                                      [else (list "unexpected end of file[@~a]"
                                                                                   (file-position /dev/stdin))])))
                                   (current-continuation-marks)
                                   null))))

(define throw-unsupported-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:read:unsupported (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                      (current-continuation-marks)
                                      null))))

(define throw-read-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                               (current-continuation-marks)
                               null))))

(define throw-signature-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:read:signature (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                    (current-continuation-marks)
                                    null))))

(define throw-syntax-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:syntax (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                 (current-continuation-marks)
                                 null))))

(define throw-check-error : (-> Input-Port (U Symbol Procedure) Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:syntax:check (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                  (current-continuation-marks)
                                  null))))

(define throw-range-error : Throw-Range-Error
  (lambda [/dev/stdin src constraint given . args]
    (define message : String (exn-args->message args "out of range"))
    (define expected : String (exn-constraint->string constraint))
    (define extended-args : (Listof Any) (list "~a~n expected: ~a~n given: ~s" message expected given))
    (raise (make-exn:syntax:range (cond [(not /dev/stdin) (exn-src+args->message src extended-args)]
                                        [else (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src extended-args))])
                                  (current-continuation-marks)
                                  null))))

(define throw-range-error* : Throw-Range-Error*
  (lambda [src constraint given . args]
    (apply throw-range-error #false src constraint given args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-name : (-> Input-Port String)
  (lambda [/dev/stdin]
    (define portname : Any (object-name /dev/stdin))
    (cond [(path? portname) (format "~a" (file-name-from-path portname))]
          [(string? portname) portname]
          [else (format "~a" portname)])))

(define exn-constraint->string : (-> Any String)
  (lambda [constraint]
    (cond [(string? constraint) constraint]
          [(procedure? constraint) (~a (object-name constraint))]
          [(list? constraint) (string-join #:before-first "{" ((inst map String Any) ~s constraint) ", " #:after-last "}")]
          [(pair? constraint) (format "[~a, ~a]" (car constraint) (cdr constraint))]
          [else (~a constraint)])))

(define exn-args->message : (-> (Listof Any) String String)
  (lambda [args defmsg]
    (cond [(null? args) defmsg]
          [(string? (car args)) (apply format (car args) (cdr args))]
          [else (~s args)])))

(define exn-src+args->message : (-> (U Symbol Procedure) (Listof Any) String)
  (lambda [src0 args]
    (define src (if (symbol? src0) src0 (assert (object-name src0) symbol?)))
    
    (if (eq? src '||)
        (cond [(null? args) ""]
              [(string? (car args)) (apply format args)]
              [else (~s args)])
        (cond [(null? args) (symbol->immutable-string src)]
              [(string? (car args)) (apply format (string-append "~a: " (car args)) src (cdr args))]
              [else (format "~a: ~s" src args)]))))
