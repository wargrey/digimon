#lang racket/base

(provide (all-defined-out)
         (rename-out [tr:read digimon-read])
         (rename-out [tr:read-syntax digimon-read-syntax])
         (rename-out [tr:get-info digimon-get-info]))

(require (prefix-in tr: typed-racket/typed-reader))
(require (prefix-in tr: typed-racket/language-info))
(require typed-racket/private/oc-button)

(define digimon-typechecking
  (let ([var "Digimon_Lang"])
    (cond [(not (eq? (system-type 'os) 'windows)) var]
          [else (string-downcase var)])))

(define digimon-smart-language
  (lambda [lang [no-check #false]]
    (define lang-value (getenv digimon-typechecking))
    (cond [(not lang-value) lang]
          [(not (string-ci=? lang-value "no-check")) lang]
          [else (or no-check (string->symbol (format "~a/no-check" lang)))])))
  
(define digimon-info
  (lambda [key default use-default]
    (case key
      [(drscheme:toolbar-buttons) (maybe-show-OC)]
      [else (use-default key default)])))
