#lang typed/racket

(provide (all-from-out typed/racket))
(provide (all-from-out "cheat.rkt" "timer.rkt" "network.rkt"))

(require "cheat.rkt")
(require "timer.rkt")
(require "network.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon 'digimon/digitama/world)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
