#lang racket/base

(provide (all-defined-out))
(provide (rename-out [texbook-command $tex]))

(require scribble/core)

(require racket/class)
(require racket/format)

(require "../../tongue.rkt")
(require "backend.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define phantomsection-style (make-style "phantomsection" null))
(define parbox-style (make-style "parbox" null)) ; for tabular to wrap line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                           (cond [(handbook-latex-renderer? get) (make-element cmd null)]
                                 [else null])))))]
      [(cmd arg)
       (make-traverse-element
        (λ [get set!]
          (cond [(handbook-latex-renderer? get) (make-element cmd (list (~a arg)))]
                [else null])))]
      [(cmd arg . args)
       (make-traverse-element
        (λ [get set!]
          (cond [(handbook-latex-renderer? get) (make-multiarg-element cmd (map ~a (cons arg args)))]
                [else null])))])))

(define texbook-command-block
  (let ([cmd0base (make-hash)])
    (case-lambda
      [(cmd)
       (hash-ref! cmd0base cmd
                  (λ [] (make-traverse-block
                         (λ [get set!]
                           (cond [(handbook-latex-renderer? get) (make-paragraph (make-style cmd null) null)]
                                 [else (make-paragraph (make-style #false null) null)])))))]
      [(cmd arg)
       (make-traverse-block
        (λ [get set!]
          (cond [(handbook-latex-renderer? get) (make-nested-flow (make-style cmd null) (if (list? arg) arg (list arg)))]
                [else arg])))]
      [(cmd arg . args)
       (make-traverse-block
        (λ [get set!]
          (cond [(handbook-latex-renderer? get) (make-nested-flow (make-style cmd null) (cons arg args))]
                [else (cons arg args)])))])))

;; Scribble ignores blocks and elements inbetween included subparts
(define texbook-command-part
  (lambda [#:part? [part? #true] #:tag [tag #false] cmd contents]
    (make-part #false
                `((part ,(or tag cmd)))
                (cond [(pair? contents) contents]
                      [(not part?) null]
                      [else (list (speak (string->symbol cmd) #:dialect 'tamer))])
               (make-style #false (if (not part?) '(unnumbered hidden toc-hidden) '(unnumbered grouper)))
               null
               (list (texbook-command-block cmd))
               null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-frontmatter
  (lambda []
    (texbook-command "frontmatter")))

(define texbook-mainmatter
  (lambda []
    (texbook-command "mainmatter")))

(define texbook-appendix
  (lambda [#:part? [part? #true] #:tag [tag #false] . contents]
    (texbook-command-part #:part? part? #:tag (or tag "tamer-appendix")
                          "appendix" contents)))

(define texbook-backmatter
  (lambda [#:part? [part? #false] #:tag [tag #false] . contents]
    (texbook-command-part #:part? part? #:tag (or tag "tamer-backmatter")
                          "backmatter" contents)))

(define texbook-onecolumn
  (lambda []
    (texbook-command-block "onecolumn")))

(define texbook-twocolumn
  (lambda []
    (texbook-command-block "twocolumn")))

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
