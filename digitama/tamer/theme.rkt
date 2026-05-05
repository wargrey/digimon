#lang racket

(provide (except-out (all-defined-out) define-theme-style make-hl:style))
(provide (rename-out [:res :thus] [:res :result] [:err :error] [:pn :delim] [:val :value]))
(provide (rename-out [:val:link :id] [:stx:link :stx] [:mod:link :mod]))
(provide (rename-out [defterm :term] [deftech :name]))

(require racket/symbol)

(require scribble/core)
(require scribble/racket)
(require scribble/manual)
(require scribble/decode)
(require scribble/html-properties)
(require scribble/private/manual-sprop)

(require "../../syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING:
; the `escape-for-tt?` means the content needs to be escaped for tt mode,
;   rather than to transform the content into tt mode.
; The tt mode is defined in latex macros, not by users' scribble code.

(define-syntax (define-theme-style stx)
  (syntax-case stx []
    [(_ id-stem [in-stem out-stem] escape-for-tt? html-class ...)
     (with-syntax ([id (make-identifier #'id-stem ":~a")])
       (syntax/loc stx
         (define (id #:decode? [decode? #true] #:theme [theme (default-theme-name)] . argv)
           (make-element (make-hl:style out-stem #false theme null)
                         (make-element (make-hl:style in-stem escape-for-tt? theme html-class ...)
                                       (if (not decode?) argv (decode-content argv)))))))]
    [(_ id-stem style-stem escape-for-tt? html-class ...)
     (with-syntax ([id (make-identifier #'id-stem ":~a")])
       (syntax/loc stx
         (define (id #:decode? [decode? #true] #:theme [theme (default-theme-name)] . argv)
           (make-element (make-hl:style style-stem escape-for-tt? theme html-class ...)
                         (if (not decode?) argv (decode-content argv))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-theme-name (make-parameter 'tamer))
(define default-theme-paths (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define :hl
  (lambda [#:decode? [decode? #true] . argv]
    (make-element highlighted-color
                  (if (not decode?) argv (decode-content argv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scribble tokens
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
(define-theme-style opt "Opt" #false) ; Brackets for optional arguments in function definitions
(define-theme-style rdr "Rdr" #true) ; reader shorthands
(define-theme-style blk "Blk" #true) ; Wrapper for multi-linked Racket code blocks

(define-theme-style val:link "ValLink" #true) ; identifiers with `for-label` binding to variable
(define-theme-style stx:link "StxLink" #true) ; identifiers with `for-label` binding to syntactic-form
(define-theme-style mod:link "ModLink" #true) ; module names linked to their definitions

(define-theme-style val:def "ValDef" #true "ValLink") ; definition for a variable, combined with `val:link`
(define-theme-style stx:def "StxDef" #true "StxLink") ; definition for a syntactic-form, combined with `stx:link`
(define-theme-style sym:def "SymDef" #true "Sym") ; definition for a unbinding identifier, combined with `sym` (maybe a mistake)

; https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide, with some detailed tokens form vim
(define-theme-style keyword "Keyword" #true) ; For tokens that represent a language keyword.
(define-theme-style namespace "Namespace" #true) ; For identifiers that declare or reference a namespace, module, or package.
(define-theme-style interface "Interface" #true) ; For identifiers that declare or reference an interface type.
(define-theme-style class "Class" #true) ; For identifiers that declare or reference a class type.
(define-theme-style struct "Struct" #true) ; For identifiers that declare or reference a struct type.
(define-theme-style type "Type" #true) ; For identifiers that declare or reference a type that is not covered above.
(define-theme-style type:parameter "TypeParameter" #true) ; For identifiers that declare or reference a type parameter.
(define-theme-style parameter "Parameter" #true) ; For identifiers that declare or reference a function or method parameters.
(define-theme-style variable "Variable" #true) ; For identifiers that declare or reference a local or global variable.
(define-theme-style property "Property" #true) ; For identifiers that declare or reference a member property, member field, or member variable.
(define-theme-style enum "Enum" #true) ; For identifiers that declare or reference an enumeration type.
(define-theme-style enum:member "EnumMember" #true) ; For identifiers that declare or reference an enumeration property, constant, or member.
(define-theme-style decorator "Decorator" #true) ; For identifiers that declare or reference decorators and annotations.
(define-theme-style event "Event" #true) ; For identifiers that declare an event property.
(define-theme-style function "Function" #true) ; For identifiers that declare a function.
(define-theme-style method "Method" #true) ; For identifiers that declare a member function or method.
(define-theme-style macro "Macro" #true) ; For identifiers that declare a macro.
(define-theme-style label "Label" #true) ; For identifiers that declare a label.
(define-theme-style comment "Comment" #true) ; For tokens that represent a comment.
(define-theme-style string "String" #true) ; For tokens that represent a string literal.
(define-theme-style character "Character" #true) ; For tokens that represent a string literal.
(define-theme-style boolean "Boolean" #true) ; For tokens that represent a boolean literal.
(define-theme-style number "Number" #true) ; For tokens that represent a number literal.
(define-theme-style regexp "Regexp" #true) ; For tokens that represent a regular expression literal.
(define-theme-style operator "Operator" #true) ; For tokens that represent an operator.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define theme-list
  (lambda [[ext #".tex"]]
    (define all
      (for*/fold ([themes null])
                 ([theme-root (in-list (remove-duplicates (cons (collection-file-path "theme" "digimon" "stone" "typeset") (default-theme-paths))))]
                  #:when (directory-exists? theme-root)
                  [theme.ext (in-list (directory-list theme-root #:build? #false))]
                  #:when (let ([maybe.ext (build-path theme-root theme.ext)])
                           (and (file-exists? maybe.ext)
                                (path-has-extension? maybe.ext ext))))
        (cons (string->symbol (path->string (path-replace-extension theme.ext #""))) themes)))
    (sort (remove-duplicates all) symbol<?)))

(define make-hl:style
  (let ([style-db (make-weak-hash)])
    (lambda [name-stem escape-for-tt? theme [maybe-class #false]]
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
                    (cond [(not escape-for-tt?) props]
                          [else (cons 'tt-chars props)])))

      (hash-ref! style-db (list theme escape-for-tt? name-stem) gen-style))))
