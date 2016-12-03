#lang racket/base

(module reader syntax/module-reader
  #:read read
  #:read-syntax read-syntax
  #:language-info '#(typed-racket/language-info get-info ())
  
  #:language
  (if (member "--no-check" (vector->list (current-command-line-arguments)))
      'typed/racket/no-check
      'typed/racket)
  
  #:info
  (λ [key default use-default]
    (case key
      [(drscheme:toolbar-buttons) (maybe-show-OC)]
      [else (use-default key default)]))

  (require typed-racket/typed-reader)
  (require typed-racket/private/oc-button))
