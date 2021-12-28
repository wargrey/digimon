#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require racket/symbol)

(require "../../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type C-Macro-Datum (U Symbol (Pairof Symbol Any) (List Symbol Any)))
(define-type (C-Config-Data a) (U a (Pairof Keyword (Listof a))))

(define-type C-Toolchain-Path (C-Config-Data Path))
(define-type C-Toolchain-Path-String (C-Config-Data Path-String))

(define-type C-Compiler-Macro (C-Config-Data C-Macro-Datum))
(define-type C-Link-Library (C-Config-Data Symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-configuration-filter : (-> (Listof Any) Symbol
                                     (Values (Listof C-Compiler-Macro) (Listof C-Toolchain-Path-String)
                                             (Listof C-Toolchain-Path-String) (Listof C-Link-Library)))
  (lambda [infos target]
    (let*-values ([(macros rest) (c-macro-filter infos target)]
                  [(includes rest) (c-include-filter rest target)]
                  [(libpaths rest) (c-libpath-filter rest target)]
                  [(libraries rest) (c-library-filter rest target)])
      (values macros includes libpaths libraries))))

(define c-path-flatten : (-> (Listof C-Toolchain-Path-String) (Listof Path))
  (lambda [dirs]
    (for/fold ([paths : (Listof Path) null])
              ([dir (in-list dirs)])
      (append paths
              (cond [(list? dir) (map path-normalize/system (cdr dir))]
                    [else (list (path-normalize/system dir))])))))

(define c-macro->string : (-> C-Compiler-Macro String)
  (lambda [D]
    (cond [(symbol? D) (format "-D~a" D)]
          [(list? D) (format "-D~a=~a" (car D) (cadr D))]
          [else (let ([maybe-keyword (car D)]
                      [maybe-macro (cdr D)])
                  (cond [(keyword? maybe-keyword) (c-macro->string maybe-macro)]
                        [else (format "-D~a=~a" maybe-keyword maybe-macro)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-macro-filter : (-> (Listof Any) Symbol (Values (Listof C-Compiler-Macro) (Listof Any)))
  (lambda [infos target]
    (define-values (sorcam tser) (c-config-filter infos '(macro) target c-macro?))
    (values (reverse sorcam) (reverse tser))))

(define c-include-filter : (-> (Listof Any) Symbol (Values (Listof C-Toolchain-Path-String) (Listof Any)))
  (lambda [infos target]
    (define-values (sedulcni tser) (c-config-filter infos '(include) target path-literal?))
    (values (reverse sedulcni) (reverse tser))))

(define c-libpath-filter : (-> (Listof Any) Symbol (Values (Listof C-Toolchain-Path-String) (Listof Any)))
  (lambda [infos target]
    (define-values (shtapbil tser) (c-config-filter infos '(libpath) target path-literal?))
    (values (reverse shtapbil) (reverse tser))))

(define c-library-filter : (-> (Listof Any) Symbol (Values (Listof C-Link-Library) (Listof Any)))
  (lambda [infos target]
    (define-values (sbil tser) (c-config-filter infos '(lib library) target symbol?))
    (values (reverse sbil) (reverse tser))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-config-filter : (All (a) (-> (Listof Any) (Listof Symbol) Symbol (-> Any Boolean : #:+ a) (Values (Listof (C-Config-Data a)) (Listof Any))))
  (lambda [infos type-aliases target datatype?]
    (let cfilter ([stnemele : (Listof (C-Config-Data a)) null]
                  [esiwrehto : (Listof Any) null]
                  [infos : (Listof Any) infos])
      (cond [(null? infos) (values (reverse stnemele) (reverse esiwrehto))]
            [else (let-values ([(self rest) (values (car infos) (cdr infos))])
                    (cond [(not (pair? self)) (cfilter stnemele esiwrehto rest)]
                          [else (let ([maybe-type (car self)])
                                  (cond [(not (symbol? maybe-type)) ; unbox (((nested-configs ...)))
                                         (if (list? self)
                                             (cfilter stnemele esiwrehto (append self rest))
                                             (cfilter stnemele esiwrehto (list* maybe-type (cdr self) rest)))]
                                        [(memq maybe-type type-aliases)
                                         (cfilter (for/fold ([stnemele : (Listof (C-Config-Data a)) stnemele])
                                                            ([elem (let ([es (cdr self)]) (if (list? es) (in-list es) (in-value es)))])
                                                    (match elem
                                                      [(? datatype? e) (cons e stnemele)]
                                                      [(list (? symbol? sys) rest ...)
                                                       #:when (or (eq? sys target) (eq? '* sys))
                                                       (c-config-element-filter rest datatype? stnemele)]
                                                      [(list (list (? symbol? ss) ...) rest ...)
                                                       #:when (memq target ss)
                                                       (c-config-element-filter rest datatype? stnemele)]
                                                      [(list (cons (? symbol? s1) (? symbol? s2)) rest ...)
                                                       #:when (or (eq? target s1) (eq? target s2))
                                                       (c-config-element-filter rest datatype? stnemele)]
                                                      [_ stnemele]))
                                                  esiwrehto rest)]
                                        [(eq? maybe-type 'config)
                                         (cfilter stnemele esiwrehto
                                                  (let ([targets (cdr self)])
                                                    (append (for/list : (Listof (Listof Any))
                                                              ([target (if (list? targets) (in-list targets) (in-value targets))] #:when (symbol? target))
                                                              (c-load-config target))
                                                            rest)))]
                                        [else (cfilter stnemele (cons self esiwrehto) rest)]))]))]))))

(define c-config-element-filter : (All (a) (-> (Listof Any) (-> Any Boolean : #:+ a) (Listof (C-Config-Data a)) (Listof (C-Config-Data a))))
  (lambda [elements datatype? stnemele]
    (for/fold ([stnemele : (Listof (C-Config-Data a)) stnemele])
              ([elem (in-list elements)])
      (cond [(datatype? elem) (cons elem stnemele)]
            [(not (pair? elem)) stnemele]
            [else (let ([tag (car elem)]
                        [rest (cdr elem)])
                    (cond [(not (keyword? tag)) (values stnemele)]
                          [(list? rest) (cons (ann (cons tag (filter datatype? rest)) (C-Config-Data a)) stnemele)]
                          [(datatype? rest) (cons (list tag rest) stnemele)]
                          [else stnemele]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-macro? : (-> Any Boolean : C-Macro-Datum)
  (lambda [datum]
    (or (symbol? datum)
        (and (pair? datum)
             (symbol? (car datum))))))

(define c-path-normalize : (-> C-Toolchain-Path-String C-Toolchain-Path)
  (lambda [path]
    (cond [(not (list? path)) (path-normalize/system path)]
          [else ((inst cons Keyword (Listof Path))
                 (car path)
                 (map path-normalize/system (cdr path)))])))

(define c-load-config : (-> Symbol (Listof Any))
  (lambda [name]
    (define rootdir : Path (collection-file-path "cc" "digimon" "stone"))
    (define name.rktl : (Option Path)
      (let ([rktl (build-path rootdir (format "~a.rktl" name))])
        (cond [(file-exists? rktl) rktl]
              [else (let ([downcase.rktl (build-path rootdir (format "~a.rktl" (string-downcase (symbol->immutable-string name))))])
                      (and (file-exists? downcase.rktl)
                           downcase.rktl))])))
    
    (or (and name.rktl
             (dynamic-require name.rktl #false)
             (let ([datum (namespace-variable-value 'cc-toolchain-config #false (λ [] #false) (module->namespace name.rktl))])
               (and (list? datum)
                    datum)))
        null)))
