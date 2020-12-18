#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/port))

(require racket/port)

(require typed/racket/random)

(require "digitama/evt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Port-Special-Datum (-> (Option Positive-Integer) (Option Natural) (Option Positive-Integer) (Option Natural) Any))
(define-type Port-Reader-Plain-Datum (U Natural EOF Input-Port (Rec x (Evtof (U Natural EOF Input-Port x)))))
(define-type Port-Reader-Datum (U Port-Special-Datum Natural EOF Input-Port (Rec x (Evtof (U Natural EOF Input-Port Port-Special-Datum x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define /dev/zero : Input-Port
  (make-input-port '/dev/zero
                   (λ [[bs : Bytes]]
                     (bytes-fill! bs #x00)
                     (bytes-length bs))
                   #false
                   void))

(define /dev/urandom : Input-Port
  (make-input-port '/dev/urandom
                   (λ [[bs : Bytes]]
                     (let ([bsize (bytes-length bs)])
                       (bytes-copy! bs 0 (crypto-random-bytes bsize))
                       bsize))
                   #false
                   void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-peek-port : (->* (Input-Port) ((Boxof Natural) Symbol) Input-Port)
  (lambda [/dev/srcin [iobox ((inst box Natural) 0)] [name '/dev/tpkin]]
    (make-input-port name
                     (λ [[s : Bytes]] : (U EOF Exact-Positive-Integer)
                       (define peeked : Natural (unbox iobox))
                       (define r (peek-bytes! s peeked /dev/srcin))
                       (set-box! iobox (+ peeked (if (number? r) r 1))) r)
                     #false
                     void)))

(define open-input-block : (->* (Input-Port Natural) (Boolean #:name Any) Input-Port)
  (lambda [/dev/srcin size [close-orig? #true] #:name [name #false]]
    (define lock-semaphore : Semaphore (make-semaphore 1))
    (define consumed : Natural 0)
    
    (define (do-read [str : Bytes]) : Port-Reader-Datum
      (define count : Integer (min (- size consumed) (bytes-length str)))
      
      (cond [(<= count 0) eof]
            [else (let ([n (read-bytes-avail!* str /dev/srcin 0 count)])
                    (cond [(eq? n 0) (wrap-evt /dev/srcin (λ (x) 0))]
                          [(number? n) (set! consumed (+ consumed n)) n]
                          [(procedure? n) (set! consumed (add1 consumed)) n]
                          [else n]))]))
    
    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (define count : Integer (min (- size consumed skip) (bytes-length str)))
      
      (cond [(<= count 0) (if (and progress-evt (sync/timeout 0 progress-evt)) #false eof)]
            [else (let ([n (peek-bytes-avail!* str skip progress-evt /dev/srcin 0 count)])
                    (cond [(not (eq? n 0)) n]
                          [(and progress-evt (sync/timeout 0 progress-evt)) #false]
                          [else (wrap-evt (cond [(zero? skip) /dev/srcin]
                                                [else (choice-evt (or progress-evt never-evt)
                                                                  (peek-bytes-evt 1 skip progress-evt /dev/srcin))])
                                          (λ [x] 0))]))]))

    (define (do-commit [n : Positive-Integer] [evt : EvtSelf] [target-evt : (Evtof Any)]) : Boolean
      (let commit ()
        (if (semaphore-try-wait? lock-semaphore)
            (let ([ok? (port-commit-peeked n evt target-evt /dev/srcin)])
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

    (make-input-port (or name (object-name /dev/srcin))
                     
                     (λ [[str : Bytes]] : Port-Reader-Datum
                       (call-with-semaphore lock-semaphore do-read try-again str))
                     
                     (λ [[str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]] : (Option Port-Reader-Datum)
                       (call-with-semaphore lock-semaphore do-peek try-again str skip progress-evt))
                     
                     (λ [] (when close-orig? (close-input-port /dev/srcin)))
                     
                     (and (port-provides-progress-evts? /dev/srcin) (λ [] (port-progress-evt /dev/srcin)))
                     (and (port-provides-progress-evts? /dev/srcin) do-commit)
                     
                     (lambda () (port-next-location /dev/srcin))
                     (lambda () (port-count-lines! /dev/srcin))

                     /dev/srcin #| initial position |#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-path : (-> (U Input-Port Output-Port) Path)
  (lambda [/dev/stdio]
    (define ?path (object-name /dev/stdio))

    (cond [(path? ?path) ?path]
          [(string? ?path) (string->path ?path)]
          [else (string->path (format "~a" ?path))])))

(define port-seek : (-> (U Input-Port Output-Port) (U Integer EOF) Natural)
  (lambda [/dev/stdio posoff]
    (cond [(exact-nonnegative-integer? posoff) (file-position /dev/stdio posoff) posoff]
          [(eof-object? posoff) (file-position /dev/stdio eof) (file-position /dev/stdio)]
          [else (file-position /dev/stdio eof)
                (let ([pos (+ (file-position /dev/stdio) posoff)])
                  (file-position /dev/stdio pos)
                  (assert pos exact-nonnegative-integer?))])))

(define port-skip : (-> Input-Port Natural Void)
  (let* ([pool-size 4096]
         [/dev/null (make-bytes pool-size)])
    (lambda [/dev/stdin n]
      (let skip ([n : Natural n])
        (define n-- (- n pool-size))
        (cond [(<= n-- 0) (read-bytes! /dev/null /dev/stdin 0 n) (void)]
              [else (read-bytes! /dev/null /dev/stdin 0 pool-size) (skip n--)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-random-access? : (-> (U Input-Port Output-Port) Boolean)
  (lambda [/dev/stdio]
    (with-handlers ([exn:fail? (λ [[e : exn]] (displayln e) #false)])
      (file-position /dev/stdio (file-position /dev/stdio))
      #true)))
