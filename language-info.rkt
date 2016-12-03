#lang racket/base

(provide (all-defined-out)
         (rename-out [tr:read digimon-read])
         (rename-out [tr:read-syntax digimon-read-syntax])
         (rename-out [tr:get-info digimon-get-info]))

(require (prefix-in tr: typed-racket/typed-reader))
(require (prefix-in tr: typed-racket/language-info))
(require typed-racket/private/oc-button)

(define digimon-smart-language
  (lambda [lang [no-check #false]]
    (if (member "--no-check" (vector->list (current-command-line-arguments)))
        (or no-check (string->symbol (format "~a/no-check" lang)))
        lang)))
  
(define digimon-info
  (lambda [key default use-default]
    (case key
      [(drscheme:toolbar-buttons) (maybe-show-OC)]
      [else (use-default key default)])))
