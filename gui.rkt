#lang typed/racket/gui

(provide (all-from-out typed/racket/gui typed/racket/snip)
         (all-from-out "main.rkt" "gui/sugar.rkt"))

(require typed/racket/snip)
(require "main.rkt" "gui/sugar.rkt")

(require/provide/syntax racket/string racket/syntax syntax/parse racket/sequence)

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon/gui 'digimon/gui/no-check)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
