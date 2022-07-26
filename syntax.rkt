#lang typed/racket/base

(provide (for-syntax (all-defined-out)))
(provide (for-syntax (all-from-out racket/base)))
(provide (for-syntax (all-from-out racket/syntax)))
(provide (for-syntax (all-from-out racket/sequence)))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/symbol))
(require (for-syntax racket/sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define (make-identifier <id> fmt)
    (format-id <id> fmt (syntax-e <id>)))
  
  (define (make-identifiers <id> <field>s [fmt "~a-~a"])
    (define id (syntax-e <id>))
    
    (for/list ([<field> (in-syntax <field>s)])
      (format-id <field> fmt id (syntax-e <field>))))
  
  (define (make-keyword-optional-arguments <field>s <DataType>s)
    (let-values ([(args reargs)
                  (for/fold ([args null] [reargs null])
                            ([<field> (in-syntax <field>s)]
                             [<DataType> (in-syntax <DataType>s)])
                    (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))]
                          [<Argument> #`[#,<field> : (Option #,<DataType>) #false]]
                          [<ReArgument> #`[#,<field> : (U Void False #,<DataType>) (void)]])
                      (values (cons <kw-name> (cons <Argument> args))
                              (cons <kw-name> (cons <ReArgument> reargs)))))])
      (list args reargs)))

  (define (make-keyword-arguments <field>s <DataType>s <defval>s)
    (let-values ([(args reargs)
                  (for/fold ([args null] [reargs null])
                            ([<field> (in-syntax <field>s)]
                             [<DataType> (in-syntax <DataType>s)]
                             [<defval> (in-syntax <defval>s)])
                    (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))]
                          [<Argument> #`[#,<field> : #,<DataType> #,@<defval>]]
                          [<ReArgument> #`[#,<field> : (U Void #,<DataType>) (void)]])
                      (values (cons <kw-name> (cons <Argument> args))
                              (cons <kw-name> (cons <ReArgument> reargs)))))])
      (list args reargs))))

