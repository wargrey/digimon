#lang racket

(provide (except-out (all-defined-out) define-theme-style make-hl:style))
(provide (rename-out [:res :thus] [:res :result] [:err :error] [:pn :delim] [:val :value] [:var :variable]))
(provide (rename-out [:val:link :id] [:stx:link :stx] [:mod:link :mod]))
(provide (rename-out [defterm :term] [deftech :name]))

(require racket/symbol)

(require scribble/core)
(require scribble/manual)
(require scribble/decode)
(require scribble/html-properties)
(require scribble/private/manual-sprop)

(require "../../syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-writing-style-theme (make-parameter 'body))

(define-syntax (define-theme-style stx)
  (syntax-case stx []
    [(_ id-stem [in-stem out-stem] default-tt? html-class ...)
     (with-syntax ([id (make-identifier #'id-stem ":~a")])
       (syntax/loc stx
         (define (id #:tt? [tt? default-tt?] #:decode? [decode? #true]
                     #:theme [theme (default-writing-style-theme)] . argv)
           (make-element (make-hl:style out-stem #false theme null)
                         (make-element (make-hl:style in-stem tt? theme html-class ...)
                                       (if (not decode?) argv (decode-content argv)))))))]
    [(_ id-stem style-stem default-tt? html-class ...)
     (with-syntax ([id (make-identifier #'id-stem ":~a")])
       (syntax/loc stx
         (define (id #:tt? [tt? default-tt?] #:decode? [decode? #true]
                     #:theme [theme (default-writing-style-theme)] . argv)
           (make-element (make-hl:style style-stem tt? theme html-class ...)
                         (if (not decode?) argv (decode-content argv))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-theme-style sym "Sym" #true) ; identifiers without `for-label` bindings
(define-theme-style in ["In" "InBG"] #true) ; standard input, also used by `litchar`
(define-theme-style out "Out" #true) ; standard output
(define-theme-style err "Err" #false) ; standard error
(define-theme-style cmt "Cmt" #true) ; comments
(define-theme-style kw "Kw" #true) ; literal "keyword" (not for `#:`-keyword), rarely used, see `pn`
(define-theme-style pn "Pn" #true) ; Parentheses, keywords, and similar delimiters in Racket code
(define-theme-style res "Res" #true) ; REPL results
(define-theme-style meta "Meta" #true) ; unquoting commas
(define-theme-style val "Val" #true) ; literal values
(define-theme-style var "Var" #true) ; local variables or meta variables
(define-theme-style opt "Opt" #true) ; Brackets for optional arguments in function definitions
; (define-theme-style mod "Mod" #true) ; a module name, use `mod:link` instead
(define-theme-style rdr "Rdr" #true) ; reader shorthands
(define-theme-style blk "Blk" #true) ; Wrapper for multi-linked Racket code blocks

(define-theme-style val:link "ValLink" #true) ; identifiers with `for-label` binding to variable
(define-theme-style stx:link "StxLink" #true) ; identifiers with `for-label` binding to syntactic-form
(define-theme-style mod:link "ModLink" #true) ; module names linked to their definitions

(define-theme-style sym:def "SymDef" #true "Sym") ; definition for a unbinding identifier, combined with `sym` (maybe a mistake)
(define-theme-style val:def "ValDef" #true "ValLink") ; definition for a variable, combined with `val:link`
(define-theme-style stx:def "StxDef" #true "StxLink") ; definition for a syntactic-form, combined with `stx:link`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-hl:style
  (let ([style-db (make-hash)])
    (lambda [name-stem tt? theme [maybe-class #false]]
      (define (gen-style)
        (define prefix (symbol->immutable-string theme))
        (define extras (if (string? maybe-class) (list maybe-class) null))

        ;; WARNING
        ; For HTML, the final value of an attribute is the last one it is defined,
        ; not based on the order it is specified in the `.class`

        (define (make-attr.class theme classes)
          (for/list ([cls (in-list classes)])
            (attributes `((class . ,(string-append theme cls))))))
        
        (define props
          (append (make-attr.class prefix extras)
                  (if (eq? theme 'Rkt) null (make-attr.class "Rkt" (cons name-stem extras)))
                  scheme-properties))
        
        (make-style (string-append prefix name-stem)
                    (cond [(not tt?) props]
                          [else (cons 'tt-chars props)])))

      (hash-ref! style-db (list theme tt? name-stem) gen-style))))
