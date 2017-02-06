#lang typed/racket/base

(provide (all-defined-out))

(require racket/function)
(require racket/fixnum)

(define clock-sequence-semaphore : Semaphore (make-semaphore 1))

(define variant+clock-sequence : (-> Positive-Fixnum)
  (lambda []
    (dynamic-wind (thunk (semaphore-wait clock-sequence-semaphore))
                  (thunk (fxior #b1000000000000000 #| ensure the N is 8, 9, A, or B |#
                                (fxand (current-memory-use) #b11111111111111)))
                  (thunk (semaphore-post clock-sequence-semaphore)))))
