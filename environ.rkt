#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require "function.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define env:sep : Bytes (if (eq? (system-type 'os) 'windows) #";" #":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define environment-variables-remove! : (-> Environment-Variables Bytes Void)
  (lambda [env name]
    (environment-variables-set! env name #false)
    (void)))

(define environment-variables-append! : (->* (Environment-Variables Bytes Bytes) ((Option Bytes)) Void)
  (lambda [env name value [sep #false]]
    (define val (environment-variables-ref env name))

    (cond [(not val) (environment-variables-set! env name value)]
          [(not sep) (environment-variables-set! env name (bytes-append val value))]
          [else (environment-variables-set! env name (bytes-append val sep value))])

    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define environment-variables-try-set! : (case-> [Environment-Variables String -> (Option (Pairof String (Option String)))]
                                                 [Environment-Variables Bytes -> (Option (Pairof Bytes (Option Bytes)))])
  (lambda [env name=value]
    (define maybe (regexp-match #px#"^\\s*(\\S+?)\\s*=\\s*(.*)$" name=value))

    (and maybe (pair? (cdr maybe)) (pair? (cddr maybe))
         (let ([nam (cadr maybe)]
               [val (caddr maybe)])
           (and (bytes-environment-variable-name? nam)
                (environment-variables-set! env nam (and val val) λfalse)
                (cond [(bytes? name=value) (cons nam val)]
                      [else (cons (bytes->string/utf-8 nam)
                                  (and val (bytes->string/utf-8 val)))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define environment-variables-push-path! : (-> Environment-Variables (Listof Path-String) [#:name Bytes] Void)
  (lambda [env #:name [name #"PATH"] paths]
    (environment-variables-append! env name
                                   (apply bytes-append
                                          (add-between (for/list : (Listof Bytes) ([p (in-list paths)])
                                                         (if (path? p) (path->bytes p) (string->bytes/utf-8 p)))
                                                       env:sep))
                                   env:sep)))
