#lang typed/racket/base

(provide (all-defined-out))

(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option vim-flags #: Vim-Flags
  #:program 'vim
  #:args srcs

  #:banner "Vi IMproved, a programmer's text editor"
  #:ps "these cmdlet demonstrates optional arguments"
  #:ps "these optional arguments can be zero"
  #:once-each
  [[(#\l)         "lisp mode"]
   [(#\L #\r)     "list swap files, with information about using them for recovery"]]
  #:multi
  [[(#\c)         command
                  "execute ~1 after the first file has been read"
                  "note: you can use up to 10 `-c`s"]]
  #:once-any
  [[(#\v)         "Vi mode"]
   [(#\e)         "Ex mode"]
   [(#\d)         "diff mode"]
   [(#\y)         "easy mode"]])

(define-values (options help? λargv) (parse-vim-flags))

(unless (not help?)
  (display-vim-flags))

(with-handlers ([exn:fail:user? (λ [e] (display-vim-flags) (exit 1))])
  (let-values ([(srcs) (λargv)])
    (list options srcs)))
