#lang typed/racket/base

(provide (all-from-out typed/racket/base))
(provide (all-from-out "digitama/sugar.rkt"))

(require "digitama/sugar.rkt")

(require/provide/syntax racket/base racket/string racket/syntax syntax/parse racket/sequence)

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language 'digimon/sugar
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
