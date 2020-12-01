#lang typed/racket/base

; https://www.hanshq.net/zip.html
; collection: file/gunzip.rkt

(provide (all-defined-out))

(require "../evt.rkt")
(require "../../port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-deflated-block : (->* (Input-Port Natural) (Boolean #:name Any) Input-Port)
  (lambda [port size [close-orig? #true] #:name [name #false]]
    (define lock-semaphore : Semaphore (make-semaphore 1))
    (define consumed : Natural 0)
    
    (define (do-read [str : Bytes]) : Port-Reader-Datum
      (define count : Integer (min (- size consumed) (bytes-length str)))
      
      (cond [(<= count 0) eof]
            [else (let ([n (read-bytes-avail!* str port 0 count)])
                    (cond [(eq? n 0) (wrap-evt port (λ (x) 0))]
                          [(number? n) (set! consumed (+ consumed n)) n]
                          [(procedure? n) (set! consumed (add1 consumed)) n]
                          [else n]))]))
    
    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (define count : Integer (min (- size consumed skip) (bytes-length str)))
      
      (cond [(<= count 0) (if (and progress-evt (sync/timeout 0 progress-evt)) #false eof)]
            [else (let ([n (peek-bytes-avail!* str skip progress-evt port 0 count)])
                    (cond [(not (eq? n 0)) n]
                          [(and progress-evt (sync/timeout 0 progress-evt)) #false]
                          [else (wrap-evt (cond [(zero? skip) port]
                                                [else (choice-evt (or progress-evt never-evt)
                                                                  (peek-bytes-evt 1 skip progress-evt port))])
                                          (λ [x] 0))]))]))

    (define (do-commit [n : Positive-Integer] [evt : EvtSelf] [target-evt : (Evtof Any)]) : Boolean
      (let commit ()
        (if (semaphore-try-wait? lock-semaphore)
            (let ([ok? (port-commit-peeked n evt target-evt port)])
              (when ok? (set! consumed (+ consumed n)))
              (semaphore-post lock-semaphore)
              ok?)
            (sync (handle-evt evt (λ [v] #false))
                  (handle-evt (semaphore-peek-evt lock-semaphore)
                              (λ [v] (commit)))))))
    
    (define (try-again) : Port-Reader-Datum
      (wrap-evt
       (semaphore-peek-evt lock-semaphore)
       (λ [x] 0)))

    (make-input-port (or name (object-name port))
                     
                     (λ [[str : Bytes]] : Port-Reader-Datum
                       (call-with-semaphore lock-semaphore do-read try-again str))
                     
                     (λ [[str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]] : (Option Port-Reader-Datum)
                       (call-with-semaphore lock-semaphore do-peek try-again str skip progress-evt))
                     
                     (λ [] (when close-orig? (close-input-port port)))
                     
                     (and (port-provides-progress-evts? port) (λ [] (port-progress-evt port)))
                     (and (port-provides-progress-evts? port) do-commit)
                     
                     (lambda () (port-next-location port))
                     (lambda () (port-count-lines! port))

                     1 #| initial position |#)))
