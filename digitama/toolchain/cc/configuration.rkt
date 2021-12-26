#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)

(require "../../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (C-Config-Data a) (U a (Pairof Keyword (Listof a))))

(define-type C-Toolchain-Path (C-Config-Data Path))
(define-type C-Toolchain-Path-String (C-Config-Data Path-String))
(define-type C-Link-Library (C-Config-Data Symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-configuration-filter : (-> (Listof Any) Symbol (Values (Listof C-Toolchain-Path-String) (Listof C-Toolchain-Path-String) (Listof C-Link-Library)))
  (lambda [infos target]
    (let*-values ([(includes rest) (c-include-filter infos target)]
                  [(libpaths rest) (c-libpath-filter rest target)]
                  [(libraries rest) (c-library-filter rest target)])
      (values includes libpaths libraries))))

(define c-path-flatten : (-> (Listof C-Toolchain-Path-String) (Listof Path))
  (lambda [dirs]
    (for/fold ([paths : (Listof Path) null])
              ([dir (in-list dirs)])
      (append paths
              (cond [(list? dir) (map path-normalize/system (cdr dir))]
                    [else (list (path-normalize/system dir))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-include-filter : (-> (Listof Any) Symbol (Values (Listof C-Toolchain-Path-String) (Listof Any)))
  (lambda [infos target]
    (c-config-filter infos '(include) target path-literal?)))

(define c-libpath-filter : (-> (Listof Any) Symbol (Values (Listof C-Toolchain-Path-String) (Listof Any)))
  (lambda [infos target]
    (c-config-filter infos '(libpath) target path-literal?)))

(define c-library-filter : (-> (Listof Any) Symbol (Values (Listof C-Link-Library) (Listof Any)))
  (lambda [infos target]
    (c-config-filter infos '(lib library) target symbol?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-config-filter : (All (a) (-> (Listof Any) (Listof Symbol) Symbol (-> Any Boolean : #:+ a) (Values (Listof (C-Config-Data a)) (Listof Any))))
  (lambda [infos type-aliases target datatype?]
    (for/fold ([stnemele : (Listof (C-Config-Data a)) null]
               [esiwrehto : (Listof Any) null])
              ([info (in-list infos)])
      (cond [(not (and (pair? info) (memq (car info) type-aliases))) (values stnemele (cons info esiwrehto))]
            [else (values (for/fold ([stnemele : (Listof (C-Config-Data a)) stnemele])
                                    ([elem (let ([es (cdr info)]) (if (list? es) (in-list es) (in-value es)))])
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
                          esiwrehto)]))))

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
(define c-path-normalize : (-> C-Toolchain-Path-String C-Toolchain-Path)
  (lambda [path]
    (cond [(not (list? path)) (path-normalize/system path)]
          [else ((inst cons Keyword (Listof Path))
                 (car path)
                 (map path-normalize/system (cdr path)))])))
