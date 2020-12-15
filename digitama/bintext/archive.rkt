#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct archive-entry
  ([src : Input-Port]
   [name : String])
  #:type-name Archive-Entry
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-archive-entry : (-> (U Bytes String Input-Port) Archive-Entry)
  (lambda [src]
    (define-values (/dev/stdin name)
      (cond [(input-port? src) (values src (archive-port-name src))]
            [(string? src) (values (open-input-string src) (symbol->immutable-string (gensym 'strin)))]
            [else (values (open-input-bytes src) (symbol->immutable-string (gensym 'bsin)))]))
    
    (archive-entry /dev/stdin name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define archive-port-name : (-> Input-Port String)
  (lambda [/dev/zipin]
    (define name (object-name /dev/zipin))

    (cond [(path? name) (path->string name)]
          [(string? name) name]
          [(symbol? name) (symbol->immutable-string name)]
          [else (format "~a" name)])))
