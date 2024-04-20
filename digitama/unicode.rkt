#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-asian-split : (-> String (Listof String))
  (lambda [s]
    (regexp-match* #px"(\\p{Lo}+)|(\\p{^Lo}+)" s)))

(define string-asian-partition : (-> String (Values (Listof String) (Listof String)))
  (lambda [s]
    (define-values (as ls)
      (for/fold ([asian : (Listof String) null]
                 [other : (Listof String) null])
                ([token (in-list (string-asian-split s))])
        (if (regexp-match? #px"^\\p{Lo}" token)
            (values (cons token asian) other)
            (values asian (cons token other)))))

    (values (reverse as) (reverse ls))))
