#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/racket)
(require scribble/latex-properties)

(require racket/list)
(require racket/symbol)

(require "misc.rkt")
(require "block.rkt")

(require "../tamer.rkt")
(require "../system.rkt")

(require "../git/lstree.rkt")
(require "../git/langstat.rkt")

(require "../../predicate.rkt")
(require "../../filesystem.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-tamer-indexed-list stx)
  (syntax-parse stx #:datum-literals []
    [(_ id
        (~alt (~optional (~seq #:anchor anchor) #:defaults ([anchor #'#true]))
              (~optional (~seq #:target-style target-style) #:defaults ([target-style #'#false])))
        ...
        [args ...] #:with [pre-flows] #:λ make-block ...)
     (with-syntax* ([tamer-id-raw (format-id #'id "tamer-~a-raw" (syntax->datum #'id))]
                    [tamer-id (format-id #'id "tamer-~a" (syntax->datum #'id))]
                    [tamer-id* (format-id #'id "tamer-~a*" (syntax->datum #'id))]
                    [tamer-id! (format-id #'id "tamer-~a!" (syntax->datum #'id))])
       (syntax/loc stx
         (define-tamer-indexed-block id #:anchor anchor #:target-style target-style
           #:with [pre-flows args ...] #:do make-block ...
           #:for [[tamer-id figure-style]
                  [tamer-id* figuremultiwide-style]
                  [tamer-id! herefigure-style]])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-code-block
  (lambda [content-style language filepath? srcpath ex-lastline? maybe-range]
    (define source (path-normalize srcpath))
    (define code (value->block source))
    (define range (code-range source maybe-range ex-lastline?))
    (define rel-srcpath (tr-d source))

    (define lang
      (value->block
       ;;; TODO, filter the language
       (cond [(string? language) language]
             [else (or (source->language source)
                       (if (symbol? language)
                           (symbol->immutable-string language)
                           (format "~a" (path-get-extension source))))])))

    (define-values (body range-start)
      (cond [(pair? range)
             (let ([range0 (number->string (car range))])
               (values (make-nested-flow code-chunk-style
                                         (list lang
                                               (value->block range0)
                                               (value->block (number->string (cdr range)))
                                               code))
                       range0))]
            [(exact-integer? range)
             (let ([range0 (number->string range)])
               (values (make-nested-flow code-tail-style
                                         (list lang
                                               (value->block range0)
                                               code))
                       range0))]
            [else (values (make-nested-flow code-file-style (list lang code)) "1")]))
    (make-nested-flow content-style
                      (cond [(not filepath?) (list body)]
                            [else (list (make-nested-flow code-title-style
                                                          (list (value->block rel-srcpath)
                                                                (make-paragraph input-color range-start)))
                                        body)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-range
  (lambda [srcpath hints ex-lastline?]
    (define (do-range srclines hint line0)
      (cond [(exact-positive-integer? hint) (values hint (if (< hint (length srclines)) (drop hint) null))]
            [(bs-regexp? hint) (search-linenumber srclines hint line0)]
            [(and (pair? hint) (bs-regexp? (car hint))) ; for overloading functions and methods
             (search-linenumber srclines (car hint) line0
                                (and (bs-regexp? (cdr hint)) (cdr hint)))]
            [else (values #false srclines)]))

    (and (pair? hints)
         (let ([srclines (path->lines* srcpath)])
           (define-values (maybe-start rest) (do-range srclines (car hints) 1))
           (cond [(null? (cdr hints)) maybe-start]
                 [(not maybe-start) maybe-start]
                 [else (let-values ([(maybe-end _) (do-range rest (cadr hints) (+ maybe-start 1))])
                         (and maybe-end
                              (< maybe-start maybe-end)
                              (cons maybe-start
                                    (if (not ex-lastline?)
                                        maybe-end
                                        (- maybe-end 1)))))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-style-extras
  (list 'multicommand
        (make-tex-addition (tamer-block-source "figure.tex"))
        (make-tex-addition (tamer-block-source "lstlisting.tex"))))

(define code-title-style (make-style "lstCodeTitle" code-style-extras))
(define code-file-style (make-style "lstFile" code-style-extras))
(define code-tail-style (make-style "lstTail" code-style-extras))
(define code-chunk-style  (make-style "lstChunk" code-style-extras))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (path-normalize srcpath)
  (cond [(relative-path? srcpath) (path-normalize (build-path (digimon-path 'zone) srcpath))]
        [else (path->string (simplify-path srcpath))]))

(define (source->language srcpath)
  (define gf (git-file (path-normalize srcpath) 0 0 #""))
  (define gl (git-files->langfiles (list gf) null #false))
  
  (and (= (hash-count gl) 1)
       (git-language-name (cdar (hash->list gl)))))

(define (value->block v)
  (make-paragraph placeholder-style v))

(define (search-linenumber srclines pattern line0 [secondary-pattern #false])
  (let search ([nl line0]
               [ls srclines])
    (and (pair? ls)
         (let-values ([(line rest) (values (car ls) (cdr ls))])
           (cond [(not (regexp-match? pattern line)) (search (+ nl 1) rest)]
                 [(not secondary-pattern) (values nl rest)]
                 [(regexp-match? secondary-pattern (car)) (values nl rest)]
                 [else (search (+ nl 1) rest)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (merge-ext+id ext id)
  (if ext (format "~a:~a" (subbytes ext 1) id) id))

(define (lang->class-range lang id)
  (case (string->symbol lang)
    [(C++) (values (pregexp (format "class\\s+~a\\s+(:[^{]+)?[{]" id)) #px"[}];")]
    [(Java) (values (format "(class|interface|enum)\\s+~a\\s+(extends[^{]+)?(implements[^{]+)?[{]" id) #px"^[}]")]
    [else (error 'lang->class-range "unsupported language: %s" lang)]))

(define (lang->function-range lang id ns)
  (case (string->symbol lang)
    [(C++) (values (pregexp (if ns (format "~a::~a\\s*[(][^)]*[)]\\s*[{]" ns id) (format "~a\\s*[(][^)]*[)]\\s*[{]" id))) #px"^[}]")]
    [(Java) (values (pregexp (format "~a\\s*[(][^)]*[)]\\s*(throws\\s+[^{]+)?[{]" id)) #px"^\\s{1,4}[}]")]
    [else (error 'lang->function-range "unsupported language: %s" lang)]))
