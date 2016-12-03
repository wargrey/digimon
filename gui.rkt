#lang typed/racket/gui

(provide (all-from-out typed/racket/gui)
         (all-from-out "cheat.rkt"))

(require "cheat.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon/gui 'digimon/digitama/worldx)
  #:language-info '#("language-info.rkt" digimon-get-info ())
  #:info digimon-info
  #:module-wrapper digimon-wrapper

  (require "language-info.rkt"))
