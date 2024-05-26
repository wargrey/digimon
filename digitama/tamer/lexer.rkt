#lang racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/match)
(require racket/sandbox)

(require setup/getinfo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-codeblock-lexer
  (lambda [lang]
    (define maybe-lang-info (read-language (open-input-string (format "#lang ~a" lang))))

    (and maybe-lang-info
         (or (maybe-lang-info 'gydm:codeblock-lexer #false)
             (maybe-lang-info 'color-lexer #false)))))

(define read-codeblock-lexer*
  (lambda [lang]
    (define maybe-lexer (read-codeblock-lexer lang))

    (values maybe-lexer
            (and maybe-lexer
                 (codeblock-lexer-info maybe-lexer)))))

(define $
  ; a module cannot have free variables...
  (make-evaluator 'stdc/cpp))

($ 'int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define codeblock-lexer-info
  (lambda [lexer-info]
    (define a:arity (procedure-arity lexer-info))
    (define r:arity (procedure-result-arity lexer-info))
    
    (displayln (procedure-arity lexer-info))
    (displayln (procedure-result-arity lexer-info))
    'whatever))
