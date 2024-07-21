#lang scribble/manual

@(require racket/promise)

@(require scribble/core)

@(define ns (module->namespace 'scribble/racket))
@(define ids (namespace-mapped-symbols ns))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@title{Highlighting Style Gallery}

@itemlist[
 (filter item?
         (for/list ([id (in-list ids)])
           (define datum (namespace-variable-value id #false (λ [] #false) ns))
           (and (promise? datum)
                (let ([s (force datum)])
                  (and (style? s)
                       (item (elem #:style s (format "~a" s))))))))
]
