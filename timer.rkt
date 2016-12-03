#lang typed/racket

(provide (all-defined-out))

(require racket/fixnum)

(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Fixnum Fixnum))))

(define current-macroseconds : (-> Fixnum)
  (lambda []
    (fl->fx (real->double-flonum (* (current-inexact-milliseconds) 1000)))))

(define timer-evt : (->* (Fixnum) (Fixnum) Timer-EvtSelf)
  (lambda [interval [basetime (current-milliseconds)]]
    (define alarm-time : Fixnum (fx+ basetime interval))
    ((inst wrap-evt Any (Vector Timer-EvtSelf Fixnum Fixnum))
     (alarm-evt alarm-time)
     (λ [alarm] (vector (timer-evt interval alarm-time) interval alarm-time)))))

(define timer-thread : (-> Fixnum (-> Thread Fixnum Any) [#:basetime Fixnum] Thread)
  (lambda [interval on-timer #:basetime [basetime (current-milliseconds)]]
    (define thdsrc : Thread (current-thread))
    (thread (thunk (let wait-dotask-loop ([evt (timer-evt interval basetime)])
                     (match (sync/enable-break evt)
                       [(vector (? evt? next-alarm) (? fixnum? interval) (? fixnum? alarm-time))
                        (on-timer thdsrc (fxquotient (fx- alarm-time basetime) interval))
                        (wait-dotask-loop next-alarm)]))))))
