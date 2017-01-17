#lang typed/racket/no-check

(provide (all-from-out typed/racket/no-check)
         (all-from-out "digitama/cheat.rkt" "digitama/main.rkt"))

(require "digitama/cheat.rkt")
(require "digitama/main.rkt")

(provide (for-syntax (all-from-out racket/base racket/string racket/syntax syntax/parse racket/sequence)))
(require (for-syntax racket/base racket/string racket/syntax syntax/parse racket/sequence))

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language 'digimon/no-check
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
