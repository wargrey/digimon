#lang typed/racket/gui

(provide (all-from-out typed/racket/gui)
         (all-from-out "main.rkt"))

(require "main.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon/gui 'digimon/digitama/worldx)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
