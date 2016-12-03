#lang racket/base

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon 'digimon/digitama/world)
  #:language-info '#("language-info.rkt" digimon-get-info ())
  #:info digimon-info

  (require "language-info.rkt"))
