#lang typed/racket/gui

(provide (all-defined-out))

(require "../cheat.rkt")

(define-type Snip (Instance Snip%))
(define-type Editor-Snip (Instance Editor-Snip%))

(define-syntax (fill-box! stx)
  (syntax-case stx [<= =]
    [(_ (w h d a t b) <= bmp) #'(fill-box! (w h d a t b) ((send bmp get-width) (send bmp get-height) 0 0 0 0))]
    [(_ (w h d a) <= bmp) #'(fill-box! (w h d a) ((send bmp get-width) (send bmp get-height) 0 0))]
    [(_ (w h) <= bmp) #'(fill-box! (w h) ((send bmp get-width) (send bmp get-height)))]
    [(_ (opbox ...) = v) #'(begin (fill-box! opbox v) ...)]
    [(_ (opbox ...) (v ...)) #'(begin (fill-box! opbox v) ...)]
    [(_ opbox v) #'(when (box? opbox) (set-box! opbox (max 0 v)))]))

(define-cheat-opaque subframe%? #:sub? Frame% frame%)

(define-cheat-opaque pasteboard%? #:is-a? Pasteboard% pasteboard%)
(define-cheat-opaque text%? #:is-a? Text% text%)
(define-cheat-opaque style-list%? #:is-a? Style-List% style-list%)
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

(define change-style : (->* ((Instance Style<%>))
                            (#:font (Option (Instance Font%))
                             #:color (Option (Instance Color%))
                             #:background-color (Option (Instance Color%)))
                            (Instance Style<%>))
  (lambda [style #:font [font #false] #:color [color #false] #:background-color [bgcolor #false]]
    (send style set-delta
          (let* ([style (make-object style-delta%)]
                 [style (if (false? color) style (send style set-delta-foreground color))]
                 [style (if (false? bgcolor) style (send style set-delta-background bgcolor))])
            (cond [(false? font) style]
                  [else (send* style
                          (set-face (send font get-face))
                          (set-family (send font get-family)))
                        (send+ style
                               (set-delta 'change-style (send font get-style))
                               (set-delta 'change-weight (send font get-weight))
                               (set-delta 'change-smoothing (send font get-smoothing))
                               (set-delta 'change-underline (send font get-underlined))
                               (set-delta 'change-size (min (exact-round (send font get-size)) 255)))])))
    style))

(define change-default-style! : (->* ((U (Instance Editor<%>) (Instance Style-List%)))
                                      (#:font (Option (Instance Font%))
                                       #:color (Option (Instance Color%))
                                       #:background-color (Option (Instance Color%)))
                                      (Instance Style<%>))
  (lambda [src #:font [font #false] #:color [color #false] #:background-color [bgcolor #false]]
    (define-values (style-list style-name)
      (cond [(text%? src) (values (send src get-style-list) (send src default-style-name))]
            [(pasteboard%? src) (values (send src get-style-list) (send src default-style-name))]
            [else (values (if (style-list%? src) src (make-object style-list%)) "Standard")]))
    (change-style #:font font #:color color #:background-color bgcolor
                  (or (send style-list find-named-style style-name)
                      (send style-list new-named-style style-name (send style-list basic-style))))))
