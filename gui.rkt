#lang typed/racket/gui

(provide (all-defined-out)
         (all-from-out typed/racket/gui typed/racket/snip)
         (all-from-out "main.rkt"))

(require typed/racket/snip)
(require "main.rkt")

(require/provide/syntax racket/string racket/syntax syntax/parse racket/sequence)

(define-cheat-opaque pasteboard%? #:is-a? Pasteboard% pasteboard%)
(define-cheat-opaque mouse%? #:is-a? Mouse-Event% mouse-event%)
(define-cheat-opaque keyboard%? #:is-a? Key-Event% key-event%)

(define-syntax (fill-box! stx)
  (syntax-case stx [<= =]
    [(_ (w h d a t b) <= bmp) #'(fill-box! (w h d a t b) ((send bmp get-width) (send bmp get-height) 0 0 0 0))]
    [(_ (w h d a) <= bmp) #'(fill-box! (w h d a) ((send bmp get-width) (send bmp get-height) 0 0))]
    [(_ (w h) <= bmp) #'(fill-box! (w h) ((send bmp get-width) (send bmp get-height)))]
    [(_ (opbox ...) = v) #'(begin (fill-box! opbox v) ...)]
    [(_ (opbox ...) (v ...)) #'(begin (fill-box! opbox v) ...)]
    [(_ opbox v) #'(when (box? opbox) (set-box! opbox (max 0 v)))]))

(module reader syntax/module-reader
  #:read digimon-read
  #:read-syntax digimon-read-syntax
  #:language (digimon-smart-language 'digimon/gui 'digimon/gui/no-check)
  #:language-info '#(digimon/language-info digimon-get-info ())
  #:info digimon-info

  (require digimon/language-info))
