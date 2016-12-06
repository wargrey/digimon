#lang typed/racket/base

(provide (all-from-out typed/racket/base))
(provide (all-from-out "digitama/sugar.rkt" "digitama/cheat.rkt"))

(require "digitama/sugar.rkt")
(require "digitama/cheat.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language 'digimon/sugar
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
