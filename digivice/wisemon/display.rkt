#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require digimon/digitama/minimal/dtrace)
(require digimon/digitama/privacy)

(unsafe-require/typed/provide
 raco/command-name
 [current-command-name (Parameterof (Option String))]
 [short-program+command-name (-> String)]
 [program+command-name (-> String)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-cmd-name : (-> Symbol)
  (lambda []
    (define cmd (current-command-name))

    (cond [(not cmd) 'wisemon]
          [else (string->symbol cmd)])))

(define wisemon-dtrace* : (-> Symbol (Option Path-String) Symbol String Any * Void)
  (lambda [action target level fmt . argl]
    (if (and target)
        (apply dtrace-message
               level (string-append "~a ~a: ~a: " fmt)
               (the-cmd-name) action (tr-d target)
               (tr-path-arguments argl))
        (apply dtrace-message
               level (string-append "~a ~a: " fmt)
               (the-cmd-name) action
               (tr-path-arguments argl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-wisemon-dtrace : (-> Symbol (Option Path-String) (-> Symbol String Any * Void))
  (lambda [action target]
    (λ [level fmt . argl]
      (apply wisemon-dtrace*
             action target
             level fmt argl))))

(define wisemon-note : (case-> [Symbol (Option Path-String) ->  Void]
                               [Symbol (Option Path-String) String Any * -> Void])
  (case-lambda
    [(action target) (wisemon-dtrace* action target 'note "")]
    [(action target fmt . argl) (apply wisemon-dtrace* action target 'note fmt argl)]))
