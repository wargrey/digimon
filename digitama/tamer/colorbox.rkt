#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/manual)

(require racket/symbol)

(require "tag.rkt")
(require "backend.rkt")

(require "../privacy.rkt")
(require "../../emoji.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-colorful-sharp-box
  (lambda [#:stroke [stroke 'ForestGreen] #:fill [fill 'MintCream] #:corner [corner 'none] content]
    (make-traverse-block
     (procedure-rename
      (λ [get set!]
        (cond [(not (handbook-latex-renderer? get)) (nested #:style named-boxed-style content)]
              [(eq? corner 'none) (make-nested-flow rounded-style
                                                    (list (colorize stroke) (colorize fill)
                                                          content))]
              [else (make-nested-flow sharp-style
                                      (list (make-paragraph plain (symbol->immutable-string corner))
                                            (colorize stroke) (colorize fill)
                                            content))]))
      'handbook-colorful-sharp-box))))
  
(define handbook-colorful-filebox
  (lambda [#:tag [tag #false] #:path-centerized? [ct? #false] latex? /path/file block]
    (define filename (tr-d /path/file))
    
    (centered
     (if (not latex?)
         (make-nested-flow (handbook-boxed-style /path/file)
                           (list (filebox (italic (string memo#) ~ filename)
                                          block)))
         (make-nested-flow (if (not ct?)
                               (handbook-filebox-style /path/file)
                               (handbook-filebox/ct-style /path/file))
                           (list (make-paragraph plain
                                                 (let ([fn (tt (smaller (emph filename)))])
                                                   (if (or tag)
                                                       (tamer-elemtag* #:type 'file
                                                                       (if (eq? tag #true) filename (format "~a" tag))
                                                                       fn)
                                                       fn)))
                                 block))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is different from 'boxed
(define named-boxed-style (make-style "boxed" (list 'multicommand 'never-indents)))
(define filebox-style (make-style "boxedFile" (list 'multicommand 'never-indents)))
(define filebox/ct-style (make-style "boxedFileCT" (list 'multicommand 'never-indents)))
(define sharp-style (make-style "SharpBox" (list 'multicommand 'never-indents)))
(define rounded-style (make-style "RoundedBox" (list 'multicommand 'never-indents)))

(define handbook-boxed-style
  (case-lambda
    [() named-boxed-style]
    [(path) (make-style (style-name named-boxed-style)
                        (cons path (style-properties named-boxed-style)))]))

(define handbook-filebox-style
  (case-lambda
    [() filebox-style]
    [(path) (make-style (style-name filebox-style)
                        (cons path (style-properties filebox-style)))]))

(define handbook-filebox/ct-style
  (case-lambda
    [() filebox/ct-style]
    [(path) (make-style (style-name filebox/ct-style)
                        (cons path (style-properties filebox/ct-style)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define colorize
  (lambda [v]
    (cond [(string? v) (make-paragraph plain v)]
          [(string? v) (make-paragraph plain (symbol->immutable-string v))]
          [else (make-paragraph plain (format "~a" v))])))
