#lang typed-racket/minimal

(provide (all-from-out typed/racket/gui typed/private/no-check-helper))
(provide (all-from-out "../no-check.rkt"))

(require racket/require typed/private/no-check-helper
         (subtract-in typed/racket/gui typed/private/no-check-helper)
         (subtract-in (except-in "../no-check.rkt" exn:fail:object) typed/private/no-check-helper))

(provide (for-syntax (all-from-out racket/base racket/string racket/syntax syntax/parse racket/sequence)))
(require (for-syntax racket/base racket/string racket/syntax syntax/parse racket/sequence))

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language 'digimon/gui/no-check
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
