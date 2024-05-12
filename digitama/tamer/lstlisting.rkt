#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/racket)
(require scribble/latex-properties)

(require racket/list)
(require racket/symbol)
(require racket/string)

(require "style.rkt")
(require "block.rkt")
(require "texbook.rkt")

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
        [args ...] #:with [legend pre-flows] #:λ make-block ...)
     (with-syntax* ([tamer-id-raw (format-id #'id "tamer-~a-raw" (syntax->datum #'id))]
                    [tamer-id (format-id #'id "tamer-~a" (syntax->datum #'id))]
                    [tamer-id* (format-id #'id "tamer-~a*" (syntax->datum #'id))]
                    [tamer-id! (format-id #'id "tamer-~a!" (syntax->datum #'id))])
       (syntax/loc stx
         (define-tamer-indexed-block id #:anchor anchor #:target-style target-style #:legend-style #false
           #:with [legend pre-flows args ...] #:do make-block ...
           #:for [[tamer-id figure-style]
                  [tamer-id* figuremultiwide-style]
                  [tamer-id! herefigure-style]])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-code-block
  (lambda [legend content-style language rootdir0 srcpath ocness maybe-range]
    (define rootdir (path-normalize rootdir0 (digimon-path 'zone)))
    (define source (path-normalize srcpath rootdir))
    (define range (code-range source maybe-range ocness))
    (define rel-srcpath (string-replace source rootdir ""))

    (define lang
      ;;; TODO, filter the language
      (cond [(string? language) (string->symbol language)]
            [else (or (source->language source rootdir)
                      (cond [(symbol? language) language]
                            [else (string->symbol (format "~a" (path-get-extension source)))]))]))

    (define-values (body range-start)
      (code-lstinputlisting lang source range))
    
    (make-nested-flow content-style
                      (list (make-nested-flow code-title-style
                                              (list (make-paragraph placeholder-style legend)
                                                    (value->block rel-srcpath)
                                                    (make-paragraph input-color range-start)
                                                    (value->block (symbol->immutable-string lang))))
                            body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-range
  (lambda [srcpath hints ocness]
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
                              (let-values ([(s e) (case ocness
                                                    [(close closed) (values maybe-start maybe-end)]
                                                    [(close-open) (values maybe-start (- maybe-end 1))]
                                                    [(open-close) (values (+ maybe-start 1) maybe-end)]
                                                    [else (values (+ maybe-start 1) (- maybe-end 1))])])
                                (and (<= s e)
                                     (cons s e)))))])))))

(define code-lstinputlisting
  (lambda [lang src range]
    (define common-args
      (list (cons 'language lang)
            'mathescape))
    
    (define-values (range-args range-start)
      (cond [(pair? range)
             (let ([range0 (number->string (car range))])
               (values (list (cons 'firstline range0)
                             (cons 'lastline (cdr range)))
                       range0))]
            [(exact-integer? range)
             (let ([range0 (number->string range)])
               (values (list (cons 'firstline range0))
                       range0))]
            [else (values null "1")]))

    (values (paragraph code-block-style
                       (texbook-command #:opt-args (list (append common-args range-args)) #:tex-only? #true
                                        "lstinputlisting" src))
            range-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-style-extras
  (list 'multicommand
        (make-tex-addition (tamer-block-source "figure.tex"))
        (make-tex-addition (tamer-block-source "lstlisting.tex"))))

(define code-title-style (make-style "lstCodeTitle" code-style-extras))
(define code-block-style (make-style "lstCodeBox" code-style-extras))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (path-normalize srcpath curdir)
  (cond [(not srcpath) (path-normalize curdir curdir)]
        [(symbol? srcpath) (digimon-path srcpath)]
        [(absolute-path? srcpath) (path->string (simplify-path srcpath))]
        [else (path-normalize (build-path curdir srcpath) curdir)]))

(define (source->language srcpath [rootdir #false])
  (define gf (make-git-file (path-normalize srcpath (or rootdir (digimon-path 'zone)))))
  (define gl (git-files->langfiles (list gf) null #false))
  
  (and (= (hash-count gl) 1)
       (string->symbol (git-language-name (cdar (hash->list gl))))))

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
(define (merge-ext+id lang ext id [cls-name #false])
  (format "~a~a"
    (if ext (string-append (bytes->string/utf-8 (subbytes ext 1)) ":") "")
    (cond [(not cls-name) id]
          [else (case (string->symbol lang)
                  [(C++) (format "~a::~a" cls-name id)]
                  [(Java) (format "~a.~a" cls-name id)]
                  [else id])])))

(define (lang->class-range lang id)
  (case (string->symbol lang)
    [(C++) (values (pregexp (format "(class|struct)\\s+~a\\s*(:[^{]+)?[{]?" id)) #px"[}];")]
    [(Java) (values (pregexp (format "(class|interface|enum)\\s+~a\\s*((extends|implements)[^{]+)?[{]?" id)) #px"^[}]")]
    [else (error 'lang->class-range "unsupported language: ~a" lang)]))

(define (lang->function-range lang id ns)
  (case (string->symbol lang)
    [(C++)
     (let ([full-id (if (not ns) (format "~a" id) (format "~a::~a" ns id))])
       (values (pregexp (string-append full-id "\\s*[(][^)]*[)]([^{]*)[{;]?")) #px"^[}]"))]
    [(Java) (values (pregexp (format "~a\\s*[(][^)]*[)]\\s*(throws\\s+[^{]+)?[{]" id)) #px"^( {1,4}|\t{1})[}]")]
    [else (error 'lang->function-range "unsupported language: ~a" lang)]))
