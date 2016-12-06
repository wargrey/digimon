#lang typed/racket

(provide (all-from-out typed/racket))
(provide (all-from-out "cheat.rkt" "digitama/main.rkt"))
(provide (all-from-out (submod "digitama/ffi.rkt" typed)))

(require "cheat.rkt")
(require "digitama/main.rkt")
(require (submod "digitama/ffi.rkt" typed))

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon 'digimon/no-check)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
