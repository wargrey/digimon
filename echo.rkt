#lang typed/racket/base

(provide Term-Color term-colorize)
(provide echof fechof eechof)

(require racket/string)
(require racket/symbol)

(require "format.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Term-Color (Option (U String Symbol Byte)))

(define-syntax (define-esc stx)
  (syntax-case stx [:]
    [(_ esc [[arg : Type defval ...] ...] fmt)
     (with-syntax ([esc* (format-id #'esc "~a*" (syntax-e #'esc))])
       (syntax/loc stx
         (begin (provide esc* esc)

                (define (esc* [arg : Type defval ...] ... #:/dev/stdout [/dev/stdout : Output-Port (current-output-port)]) : Void
                  (fprintf /dev/stdout fmt arg ...))

                (define (esc [arg : Type defval ...] ... #:/dev/stdout [/dev/stdout : Output-Port (current-output-port)]) : Void
                  (when (terminal-port? /dev/stdout)
                    (esc* #:/dev/stdout /dev/stdout arg ...))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define named-colors : (Immutable-HashTable Symbol Byte)
  #hasheq((black . 0) (darkgray . 8) (darkgrey . 8) (lightgray . 7) (lightgrey . 7) (gray . 7) (grey . 7) (white . 15)
                      (darkred . 1) (darkgreen . 2) (darkyellow . 3) (darkblue . 4) (brown . 5) (darkmagenta . 5)
                      (darkcyan . 6) (red . 9) (lightred . 9) (green . 10) (lightgreen . 10) (yellow . 11) (lightyellow . 11)
                      (blue . 12) (lightblue . 12) (magenta . 13) (lightmagenta . 13) (cyan . 14) (lightcyan . 14)))

(define term-colorize : (-> Term-Color Term-Color (Listof Symbol) String String)
  (lambda [fg bg attrs content]
    (define (color-code [color : (U Byte String Symbol)] [bg? : Boolean]) : String
      (format "~a8;5;~a"
        (if bg? 4 3)
        (cond [(symbol? color)
               (hash-ref named-colors color
                         (λ [] (hash-ref named-colors (string->symbol (string-downcase (symbol->immutable-string color)))
                                         (λ [] 0))))]
              [(string? color) (hash-ref named-colors (string->symbol (string-downcase color)) (λ [] 0))]
              [else color])))
    
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                      (string-replace (for/fold : String ([effects ""]) ([attr : Symbol (in-list attrs)])
                                        (case (string-downcase (symbol->immutable-string attr))
                                          [{"bold" "bright"} (string-append effects ";1")]
                                          [{"dim"} (string-append effects ";2")]
                                          [{"underline" "undercurl"} (string-append effects ";4")]
                                          [{"blink"} (string-append effects ";5")]
                                          [{"reverse" "inverse"} (string-append effects ";7")]
                                          [{"hidden" "password"} (string-append effects ";8")]
                                          [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                      "^;" "" #:all? #false)
                      (if (not fg) 39 (color-code fg #false))
                      (if (not bg) 49 (color-code bg #true))))))

(define term-echo : (-> Output-Port String Term-Color Term-Color (Listof Symbol) Void)
  (lambda [/dev/stdout rawmsg fg bg attrs]
    (display (if (terminal-port? /dev/stdout) (term-colorize fg bg attrs rawmsg) rawmsg)
             /dev/stdout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (term-echo (current-output-port) (~string msgfmt vals) fg bg attrs)))

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (term-echo (current-error-port) (~string msgfmt vals) fg bg attrs)))

(define fechof : (-> Output-Port String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [/dev/stdout msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg : String (~string msgfmt vals))
    (define colorize? (terminal-port? /dev/stdout))

    (display (if colorize? (term-colorize fg bg attrs rawmsg) rawmsg)
             /dev/stdout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-esc esc-save [] "\033[s")
(define-esc esc-restore [] "\033[u")

(define-esc esc-screen-home [] "\033[H") ; cell(0, 0)
(define-esc esc-cell [[row : Integer 0] [col : Integer 0]] "\033[~a;~af")
(define-esc esc-move-up [[line : Integer 1]] "\033[~aA")
(define-esc esc-move-down [[line : Integer 1]] "\033[~aB")
(define-esc esc-move-right [[col : Integer 1]] "\033[~aC")
(define-esc esc-move-left [[col : Integer 1]] "\033[~aD")
(define-esc esc-return-down [[line : Integer 1]] "\033[~aE")
(define-esc esc-return-up [[line : Integer 1]] "\033[~aF")
(define-esc esc-move-to [[col : Integer 0]] "\033[~aG")
(define-esc esc-home [] "\033[0G") ; move-to(0)

(define-esc esc-clear-screen-to-end [] "\033[J")
(define-esc esc-clear-screen-from-beginning [] "\033[1J")
(define-esc esc-clear-screen [] "\033[2J")
(define-esc esc-clear-line-to-end [] "\033[K")
(define-esc esc-clear-line-from-beginning [] "\033[1K")
(define-esc esc-clear-line [] "\033[2K")
