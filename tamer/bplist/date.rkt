#lang typed/racket

(require digimon/plist)

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [seconds->date (->* (Real) (Boolean) date)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define display? : Boolean #true)

;; WARNING: (current-date) returns local date which is not expected by bplist
(define plist : PList-Datum
  (seconds->date (* 0.001 (current-inexact-milliseconds)) #false))
