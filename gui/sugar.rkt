#lang typed/racket/gui

(provide (all-defined-out))

(require "../cheat.rkt")

(define-syntax (fill-box! stx)
  (syntax-case stx [<= =]
    [(_ (w h d a t b) <= bmp) #'(fill-box! (w h d a t b) ((send bmp get-width) (send bmp get-height) 0 0 0 0))]
    [(_ (w h d a) <= bmp) #'(fill-box! (w h d a) ((send bmp get-width) (send bmp get-height) 0 0))]
    [(_ (w h) <= bmp) #'(fill-box! (w h) ((send bmp get-width) (send bmp get-height)))]
    [(_ (opbox ...) = v) #'(begin (fill-box! opbox v) ...)]
    [(_ (opbox ...) (v ...)) #'(begin (fill-box! opbox v) ...)]
    [(_ opbox v) #'(when (box? opbox) (set-box! opbox (max 0 v)))]))

(define-cheat-opaque pasteboard%? #:is-a? Pasteboard% pasteboard%)
(define-cheat-opaque mouse%? #:is-a? Mouse-Event% mouse-event%)
(define-cheat-opaque keyboard%? #:is-a? Key-Event% key-event%)

(define default.cur : (Instance Cursor%) (make-object cursor% 'arrow))
(define blank.cur : (Instance Cursor%) (make-object cursor% 'blank))
(define watch.cur : (Instance Cursor%) (make-object cursor% 'watch))
(define bullseye.cur : (Instance Cursor%) (make-object cursor% 'bullseye))
(define cross.cur : (Instance Cursor%) (make-object cursor% 'cross))
(define hand.cur : (Instance Cursor%) (make-object cursor% 'hand))
(define ibeam.cur : (Instance Cursor%) (make-object cursor% 'ibeam))
(define size-e/w.cur : (Instance Cursor%) (make-object cursor% 'size-e/w))
(define size-n/s.cur : (Instance Cursor%) (make-object cursor% 'size-n/s))
(define size-ne/sw.cur : (Instance Cursor%) (make-object cursor% 'size-ne/sw))
(define size-nw/se.cur : (Instance Cursor%) (make-object cursor% 'size-nw/se))
