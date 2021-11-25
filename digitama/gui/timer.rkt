#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/gui)

(require racket/fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type GameTimer%
  (Class #:implements Timer%
         (init-field [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Any)]
                     [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Any) #:optional])))

(define game-timer% : GameTimer%
  (class timer%
    (super-new [just-once? #false] [interval #false] [notify-callback void])

    (init-field on-elapse)
    (init-field [on-elapsed void])
    
    (define timer-interval : Nonnegative-Fixnum 0)
    (define uptime : Nonnegative-Fixnum 0)
    (define keep-running? : Boolean #true)

    (define/override (start frame-rate [just-once? #false])
      (set-frame-rate (if (= frame-rate 0) 60 frame-rate) just-once?)
      (super start timer-interval #true)
      (set! uptime timer-interval))

    (define/override (notify)
      (define elapse0 : Flonum (current-inexact-milliseconds))

      (on-elapse timer-interval uptime)

      (define elapsed : Fixnum (fl->fx (round (- (current-inexact-milliseconds) elapse0))))

      (unless (eq? elapsed void)
        (on-elapsed timer-interval uptime elapsed))
      
      (unless (not keep-running?)
        (define this-interval : Integer (- timer-interval elapsed))
        
        (set! uptime (fx+ uptime timer-interval))

        (cond [(>= this-interval 0) (super start this-interval #true)]
              [else (let time-skip ([next-tick : Fixnum (fx+ this-interval timer-interval)])
                      (set! uptime (fx+ uptime timer-interval))
                      (cond [(>= next-tick 0) (super start next-tick #true)]
                            [else (time-skip (fx+ next-tick timer-interval))]))])))

    (define/override (interval)
      timer-interval)

    (define (set-frame-rate [rate : Integer] [just-once? : Any]) : Void
      (set! timer-interval (if (> rate 0) (fxquotient 1000 rate) (fx* 1000 (- rate))))
      (set! keep-running? (not just-once?)))))
