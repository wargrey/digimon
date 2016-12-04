#lang typed/racket

(provide (all-defined-out))
  
(define clock-sequence-semaphore : Semaphore (make-semaphore 1))

(define variant+clock-sequence : (-> Natural)
  (lambda []
    (dynamic-wind (thunk (semaphore-wait clock-sequence-semaphore))
                  (thunk (+ #b1000000000000000 #| ensure the N is 8, 9, A, or B |#
                            (remainder (current-memory-use) #b11111111111111)))
                  (thunk (semaphore-post clock-sequence-semaphore)))))
