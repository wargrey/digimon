#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Syn-Token-Stdin JSON-Stdin]))

(require typed/syntax/readerr)

(require "../token.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type JSON-Datum (U Void Boolean String Integer Flonum))
(define-type JSExpr (Rec p (U JSON-Datum (Listof p) (HashTable Symbol p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define json-stdin->port : (-> Syn-Token-Stdin Input-Port)
  (lambda [/dev/stdin]
    (syn-token-stdin->port /dev/stdin #px"\\.json$" 'jsoin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define json-extract-value : (-> Input-Port JSExpr)
  (lambda [/dev/jsoin]
    (define maybe-delim : (U Char EOF) (syn-token-peek-char /dev/jsoin))
    
    (cond [(eof-object? maybe-delim) (json-error /dev/jsoin "unexpected end of file!" maybe-delim)]
          [(eq? maybe-delim #\{) (read-char /dev/jsoin) (json-extract-object /dev/jsoin)]
          [(eq? maybe-delim #\[) (read-char /dev/jsoin) (json-extract-array /dev/jsoin)]
          [else (let ([datum (read /dev/jsoin)])
                  (cond [(string? datum) datum]
                        [(exact-integer? datum) datum]
                        [(real? datum) (real->double-flonum datum)]
                        [(eq? datum 'true) #true]
                        [(eq? datum 'false) #false]
                        [(eq? datum 'null) (void)]
                        [else (json-error /dev/jsoin "invalid JSON value" datum)]))])))

(define json-extract-object : (-> Input-Port (HashTable Symbol JSExpr))
  (let ([js-object : (HashTable Symbol JSExpr) (hasheq)])
    (lambda [/dev/jsoin]
      (define maybe-delim : (U Char EOF) (syn-token-peek-char /dev/jsoin))

      (cond [(eq? maybe-delim #\}) (read-char /dev/jsoin) js-object]
            [(not (eq? maybe-delim #\")) (json-error /dev/jsoin "invalid name in reading JSON object" (read /dev/jsoin))]
            [else (let extract-object ([object : (HashTable Symbol JSExpr) js-object])
                    (define str-name (read /dev/jsoin))
                    (define delim (read /dev/jsoin))

                    (cond [(not (eq? delim ':)) (json-error /dev/jsoin "invalid delimiter in reading JSON object" delim)]
                          [else (let ([value (json-extract-value /dev/jsoin)]
                                      [name (string->symbol (assert str-name string?))]
                                      [sep (syn-token-read-char /dev/jsoin)])
                                  (cond [(eq? sep #\,) (extract-object (hash-set object name value))]
                                        [(eq? sep #\}) (hash-set object name value)]
                                        [else (json-error /dev/jsoin "invalid separator in reading JSON object" sep)]))]))]))))

(define json-extract-array : (-> Input-Port (Listof JSExpr))
  (lambda [/dev/jsoin]
    (define maybe-delim : (U Char EOF) (syn-token-peek-char /dev/jsoin))

    (cond [(eq? maybe-delim #\]) (read-char /dev/jsoin) null]
          [else (let extract-array ([array : (Listof JSExpr) null])
                  (define value : JSExpr (json-extract-value /dev/jsoin))
                  (define sep : (U Char EOF) (syn-token-read-char /dev/jsoin))
                  
                  (cond [(eq? sep #\,) (extract-array (cons value array))]
                        [(eq? sep #\]) (reverse (cons value array))]
                        [else (json-error /dev/jsoin "invalid separator in reading JSON array" sep)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define json-error : (-> Input-Port String Any Nothing)
  (lambda [/dev/jsoin message value]
    (define src : Any (object-name /dev/jsoin))
    (define-values (line col pos) (port-next-location /dev/jsoin))

    (if (eof-object? value)
        (raise-read-eof-error message src line col pos #false)
        (raise-read-error (format "~a: ~a" message value) src line col pos #false))))
