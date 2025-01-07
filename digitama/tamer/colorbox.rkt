#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/manual)

(require racket/symbol)

(require "tag.rkt")
(require "backend.rkt")

(require "../tamer.rkt")
(require "../../emoji.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-colorful-sharp-box
  (lambda [#:stroke [stroke 'ForestGreen] #:fill [fill 'MintCream] #:corner [corner 'none] content]
    (make-traverse-block
     (λ [get set!]
       (cond [(not (handbook-latex-renderer? get)) (nested #:style named-boxed-style content)]
             [(eq? corner 'none) (make-nested-flow rounded-style
                                                   (list (colorize stroke) (colorize fill)
                                                         content))]
             [else (make-nested-flow sharp-style
                                     (list (make-paragraph plain (symbol->immutable-string corner))
                                           (colorize stroke) (colorize fill)
                                           content))])))))

(define handbook-colorful-filebox
  (lambda [#:tag [tag #false] latex? /path/file block]
    (define filename (path->string (tr-if-path /path/file)))
    
    (centered
     (if (not latex?)
         (make-nested-flow (handbook-boxed-style /path/file)
                           (list (filebox (italic (string memo#) ~ filename)
                                          block)))
         (make-nested-flow (handbook-filebox-style /path/file)
                           (list (make-paragraph plain (tamer-elemtag* #:type 'file (if (not tag) filename (format "~a" tag))
                                                                       (tt (smaller (emph filename)))))
                                 block))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is different from 'boxed
(define named-boxed-style (make-style "boxed" (list 'multicommand 'never-indents)))
(define filebox-style (make-style "boxedFile" (list 'multicommand 'never-indents)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define colorize
  (lambda [v]
    (cond [(string? v) (make-paragraph plain v)]
          [(string? v) (make-paragraph plain (symbol->immutable-string v))]
          [else (make-paragraph plain (format "~a" v))])))
