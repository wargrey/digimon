#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct c-modeline () #:type-name C-Modeline)

(struct c:mdl:ld c-modeline
  ([keyword : (Option Symbol)]
   [libraries : (Listof String)])
  #:constructor-name make-c:mdl:ld
  #:type-name C-LD-Modeline
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-source-modelines : (-> Path-String (Listof C-Modeline))
  (lambda [c]
    (call-with-input-file* c
      (λ [[/dev/cin : Input-Port]]
        (let read-modeline : (Listof C-Modeline) ([seniledom : (Listof C-Modeline) null])
          (define line (read-line /dev/cin 'any))
          (cond [(eof-object? line) (reverse seniledom)]
                [(regexp-match? #px"[Ee][Nn][Dd]\\s+[Oo][Ff]\\s+[Mm][Oo][Dd][Ee][Ll][Ii][Nn][Ee]" line) (reverse seniledom)]
                [(not (regexp-match? #px"#include\\s+<" line)) (read-modeline seniledom)]
                [else (match (regexp-match #px".+ld:(\\w+)?:?\\s*([^*]+)(\\*/)?\\s*$" line)
                        [(list _ maybe-keyword (? string? ls) _)
                         (let ([keyword (and maybe-keyword (string->symbol maybe-keyword))]
                               [libraries (string-split ls)])
                           (cond [(null? libraries) (read-modeline seniledom)]
                                 [else (read-modeline (cons (make-c:mdl:ld keyword libraries) seniledom))]))]
                        [_ (read-modeline seniledom)])]))))))
