#lang typed/racket

(provide (all-from-out typed/racket)
         (all-from-out "cheat.rkt"))

(require "cheat.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon 'digimon/digitama/world)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
