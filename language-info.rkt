#lang racket/base

(provide (all-defined-out)
         (rename-out [tr:read digimon-read])
         (rename-out [tr:read-syntax digimon-read-syntax])
         (rename-out [tr:get-info digimon-get-info]))

(require (prefix-in tr: typed-racket/typed-reader))
(require (prefix-in tr: typed-racket/language-info))

(define digimon-lang
  (let ([var "DIGIMON_LANG"])
    (cond [(not (eq? (system-type 'os) 'windows)) var]
          [else (string-downcase var)])))

(define digimon-smart-language
  (let ([cache (box #false)])
    (lambda [lang [no-check #false]]
      (define lang-value
        (or (unbox cache)
            (let ([v (getenv digimon-lang)])
              (cond [(not v) 'check]
                    [else (let ([sym (string->symbol (string-downcase v))])
                            (set-box! cache sym) sym)]))))
      (cond [(not (eq? lang-value 'no-check)) lang]
            [else (or no-check (string->symbol (format "~a/no-check" lang)))]))))
  
(define digimon-info
  (lambda [key default use-default]
    (case key
      [(drracket:opt-in-toolbar-buttons) '(optimization-coach)]
      [else (use-default key default)])))
