#lang typed/racket

(require digimon/plist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define display? : Boolean #true)

;; WARNING: (current-date) returns local date which is not expected by bplist
(define plist : PList-Datum
  (seconds->date (* 0.001 (current-inexact-milliseconds)) #false))
