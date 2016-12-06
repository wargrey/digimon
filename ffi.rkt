#lang racket

(provide (all-from-out racket))
(provide (all-from-out "digitama/ffi.rkt" "digitama/main.rkt"))

(require "digitama/ffi.rkt")
(require "digitama/main.rkt")

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language 'digimon/ffi
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
