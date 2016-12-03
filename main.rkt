#lang typed/racket

(provide (all-from-out typed/racket))
(provide (all-from-out "cheat.rkt" "main.rkt"))

(require "cheat.rkt")
(require "main.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon 'digimon/digitama/world)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
