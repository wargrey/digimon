#lang racket/base

(provide (all-defined-out))
(provide (rename-out [texbook-command $tex]))

(require scribble/core)

(require racket/class)
(require racket/format)

(require "../tamer.rkt")
(require "../../tongue.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define phantomsection-style (make-style "phantomsection" null))

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

(define texbook-command-block
  (let ([cmd0base (make-hash)])
    (case-lambda
      [(cmd)
       (hash-ref! cmd0base cmd
                  (λ [] (make-traverse-block
                         (λ [get set!]
                           (cond [(texbook-renderer? get) (make-paragraph (make-style cmd null) null)]
                                 [else (make-paragraph (make-style #false null) null)])))))]
      [(cmd arg)
       (make-traverse-block
        (λ [get set!]
          (cond [(texbook-renderer? get) (make-nested-flow (make-style cmd null) (if (list? arg) arg (list arg)))]
                [else arg])))]
      [(cmd arg . args)
       (make-traverse-block
        (λ [get set!]
          (cond [(texbook-renderer? get) (make-nested-flow (make-style cmd null) (cons arg args))]
                [else (cons arg args)])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-front
  (lambda []
    (texbook-command "frontmatter")))

(define texbook-main
  (lambda []
    (texbook-command "mainmatter")))

;; Scribble ignores blocks and elements inbetween included subparts
(define texbook-appendix
  (lambda [#:part? [part? #true] #:tag [tag #false] . contents]
    (make-part #false
                `((part ,(or tag "tamer-appendix")))
               (if (pair? contents) contents (list (speak 'appendix #:dialect 'tamer)))
               (make-style #false (if (not part?) '(unnumbered hidden toc-hidden) '(unnumbered grouper)))
               null
               (list (texbook-command-block "appendix"))
               null)))

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
