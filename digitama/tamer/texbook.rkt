#lang racket/base

(provide (all-defined-out))
(provide (rename-out [texbook-command $tex]))

(require scribble/core)

(require racket/class)
(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-renderer?
  (lambda [get]
    (and (memq 'latex
               (cond [(procedure? get) (get 'scribble:current-render-mode null)]
                     [else (send get current-render-mode)]))
         #true)))

(define texbook-prefab-name
  (lambda [TeX]
    (case (string->symbol (string-downcase TeX))
      [(tex) (texbook-command "TeX")]
      [(latex) (texbook-command "LaTeX")]
      [(latexe) (texbook-command "LaTeXe")]
      [else (texbook-command TeX)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-command
  (let ([cmd0base (make-hash)])
    (case-lambda
      [(cmd)
       (hash-ref! cmd0base cmd
                  (λ [] (make-traverse-element
                         (λ [get set!]
                           (cond [(texbook-renderer? get) (make-element cmd null)]
                                 [else null])))))]
      [(cmd arg)
       (make-traverse-element
        (λ [get set!]
          (cond [(texbook-renderer? get) (make-element cmd (list (~a arg)))]
                [else null])))]
      [(cmd arg . args)
       (make-traverse-element
        (λ [get set!]
          (cond [(texbook-renderer? get) (make-multiarg-element cmd (map ~a (cons arg args)))]
                [else null])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-front
  (lambda []
    (texbook-command "frontmatter")))

(define texbook-main
  (lambda []
    (texbook-command "mainmatter")))

(define texbook-appendix
  (lambda []
    (texbook-command "appendix")))

(define texbook-back
  (lambda []
    (texbook-command "backmatter")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $tex:phantomsection
  (lambda []
    (texbook-command "phantomsection")))

(define $tex:newcounter
  (lambda [name]
    (texbook-command "newcounter" name)))

(define $tex:refstepcounter
  (lambda [counter]
    (texbook-command "refstepcounter" counter)))

(define $tex:setcounter
  (lambda [counter value]
    (texbook-command "setcounter" counter value)))
