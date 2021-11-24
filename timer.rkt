#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/match)

(require "digitama/evt.rkt")
(require "number.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define timer-evt : (->* (Nonnegative-Fixnum) (Fixnum) Timer-EvtSelf)
  (lambda [interval [basetime (current-milliseconds)]]
    (let nested ([prev-tick : Nonnegative-Fixnum (fxmax 0 basetime)]
                 [uptime : Nonnegative-Fixnum interval])
      (define alarm-time : Nonnegative-Fixnum (fx+ prev-tick interval))
      ((inst wrap-evt Any (Immutable-Vector Timer-EvtSelf Nonnegative-Fixnum Nonnegative-Fixnum))
       (alarm-evt alarm-time)
       (λ [alarm] (vector-immutable (nested alarm-time (fx+ uptime interval)) interval uptime))))))

(define timer-thread : (-> Nonnegative-Fixnum (-> Thread Nonnegative-Fixnum Nonnegative-Fixnum Any) [#:basetime Fixnum] [#:tolerance Real] Thread)
  (lambda [interval on-timer #:basetime [basetime (current-milliseconds)] #:tolerance [tolerance 16.0]]
    (define fltolerance : Flonum (real->double-flonum tolerance))
    (define thdsrc : Thread (current-thread))
    
    (thread (λ [] (with-handlers ([exn? void])
                    (let wait-dotask-loop ([alarm-time : Nonnegative-Fixnum (fx+ (fxmax 0 basetime) interval)]
                                           [uptime : Nonnegative-Fixnum interval])
                      (sync/enable-break (alarm-evt alarm-time))
                      (on-timer thdsrc interval uptime)

                      (define now : Flonum (- (current-inexact-milliseconds) fltolerance))
                      (let time-skip ([atime++ : Nonnegative-Fixnum (fx+ alarm-time interval)]
                                      [uptime++ : Nonnegative-Fixnum (fx+ uptime interval)])
                        (cond [(>= atime++ now) (wait-dotask-loop atime++ uptime++)]
                              [else (time-skip (fx+ atime++ interval) (fx+ uptime++ interval))]))))))))
