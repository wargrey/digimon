#lang typed/racket/base

(provide (all-defined-out))

(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-silents : (Listof Exec-Silent) (list 'stdout 'stderr))

(define git:%at : String "--pretty=format:%at")
(define px:rename:normal : Regexp #px"[{]?([^{]+) => ([^}]+)[}]?")
(define px:rename:sub : Regexp #px"/[{] => ([^}]+)[}]")
(define px:rename:sup : Regexp #px"[{]([^{]+) => [}]/")

(define px:submodule:status : Regexp #px"^\\s?\\S+\\s([^(]+)\\s[(][^)]+[)]$")

(define px:spaced-fname:numstat : Regexp #px"^\\d+\\s+\\d+\\s+(.+)$")
(define px:spaced-fname:ls-tree : Regexp #px"^\\d+\\s+\\w+\\s+[[:xdigit:]]+\\s+\\S+\\s+(.+)$")

(define current-git-procedure : (Parameterof Any) (make-parameter #false))
