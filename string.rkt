#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/string))

(require racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-uri? : (-> String Boolean)
  (let ([rx:3986 #rx"^(?:([^:/?#]*):)?(?://(?:([^/?#@]*)@)?(?:(?:\\[([0-9a-fA-F:]*:[0-9a-fA-F:]*)\\])|([^/?#:]*))?(?::([0-9]*))?)?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$"])
    (lambda [uri]
      (regexp-match? rx:3986 uri))))
