#lang typed/racket/base

(provide (all-defined-out))

(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-silents : (Listof Exec-Silent) (list 'stdout 'stderr))
(define-type Git-Match-Datum (U Regexp Byte-Regexp String (Listof (U Regexp Byte-Regexp String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git:%at : String "--pretty=format:%at")
(define px:rename:normal : Regexp #px"[{]?([^{]+) => ([^}]+)[}]?")
(define px:rename:sub : Regexp #px"/[{] => ([^}]+)[}]")
(define px:rename:sup : Regexp #px"[{]([^{]+) => [}]/")

(define px:submodule:status : Regexp #px"^.(\\S+)\\s([^(]+)\\s[(][^)]+[)]$")

(define px:spaced-fname:numstat : Regexp #px"^\\d+\\s+\\d+\\s+(.+)$")
(define px:spaced-fname:ls-tree : Regexp #px"^\\d+\\s+\\w+\\s+[[:xdigit:]]+\\s+\\S+\\s+(.+)$")

(define current-git-procedure : (Parameterof Any) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-path-match? : (->* (String Git-Match-Datum) (#:match-for-empty? Boolean) Boolean)
  (lambda [name pattern #:match-for-empty? [null-value #false]]
    (cond [(regexp? pattern) (regexp-match? pattern name)]
          [(string? pattern) (string=? pattern name)]
          [(byte-regexp? pattern) (regexp-match? pattern name)]
          [(null? pattern) null-value]
          [else (for/or : Boolean ([p (in-list pattern)])
                  (git-path-match? name p))])))
