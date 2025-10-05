#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/port))
(provide (all-from-out "digitama/minimal/port.rkt"))
(provide EvtSelf port-always-write-evt port-always-write-special-evt)

(provide Output-Gate output-gate? make-output-gate)
(provide Output-Gate<%> output-gate<%>? make-output-gate<%>)
(provide Output-Gate-Write Output-Gate-Flush Output-Gate-Close Output-Gate-Position0)
(provide (rename-out [output-gate-port open-output-gate]))

(require racket/port)

(require typed/racket/random)

(require "digitama/unsafe/ops.rkt")
(require "digitama/evt.rkt")
(require "digitama/port.rkt")

(require "digitama/minimal/port.rkt")
(require "digitama/minimal/system.rkt")

(require "format.rkt")
(require "function.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Port-Special-Datum (-> (Option Positive-Integer) (Option Natural) (Option Positive-Integer) (Option Natural) Any))
(define-type Port-Reader-Plain-Datum (U Natural EOF Input-Port (Rec x (Evtof (U Natural EOF Input-Port x)))))
(define-type Port-Reader-Datum (U Port-Special-Datum Natural EOF Input-Port (Rec x (Evtof (U Natural EOF Input-Port Port-Special-Datum x)))))
(define-type Port-Chain-Block-Info (-> Index (Values Natural Natural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (lambda [/dev/srcin size [close-orig? #false] #:name [name #false]]
    (define lock-semaphore : Semaphore (make-semaphore 1))
    (define consumed : Natural 0)
    
    (define (do-read [str : Bytes]) : Port-Reader-Datum
      (define available : Integer (- size consumed))
      
      (if (> available 0)
          (let ([n (read-bytes-avail!* str /dev/srcin 0 (min available (bytes-length str)))])
            (cond [(eq? n 0) (wrap-evt /dev/srcin (λ (x) 0))]
                  [(number? n) (set! consumed (+ consumed n)) n]
                  [(procedure? n) #| special values are not counted in as consumed bytes |# n]
                  [else n]))
          eof))
    
    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (define count : Integer (min (- size consumed skip) (bytes-length str)))
      
      (if (> count 0)
          (let ([n (peek-bytes-avail!* str skip progress-evt /dev/srcin 0 count)])
            (cond [(not (eq? n 0)) n]
                  [(and progress-evt (sync/timeout 0 progress-evt)) #false]
                  [else (wrap-evt (if (> skip 0)
                                      (choice-evt (or progress-evt never-evt)
                                                  (peek-bytes-evt 1 skip progress-evt /dev/srcin))
                                      /dev/srcin)
                                  (λ [x] 0))]))
          (if (and progress-evt (sync/timeout 0 progress-evt))
              #false eof)))

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

(define open-input-block-chain : (->* (Input-Port (U (Pairof (Listof Index) Port-Chain-Block-Info)
                                                     (Listof (Pairof Natural Natural))))
                                      (Boolean #:name Any #:total (Option Natural) #:force-thread-safe? Boolean)
                                      Input-Port)
  (lambda [#:total [maybe-total #false] #:name [name #false] #:force-thread-safe? [thread-safe? #false]
           /dev/srcin block-chain [close-orig? #false]]
    (define lock-semaphore : Semaphore (make-semaphore 1))
    (define block-count : Index (if (list? block-chain) (length block-chain) (length (car block-chain))))
    (define block-positions : (Vectorof Natural) (make-vector block-count))
    (define block+sizes : (Vectorof Natural) (make-vector block-count))
    (define consumed : Natural 0)
    (define block-idx : Index 0)

    (define max-size : Natural
      (if (list? block-chain)
          (for/fold ([acc-size : Natural 0])
                    ([p.s (in-list block-chain)]
                     [idx (in-naturals 0)])
            (define size++ (+ acc-size (cdr p.s)))
            (vector-set! block-positions idx (car p.s))
            (vector-set! block+sizes idx size++)
            size++)
          (for/fold ([acc-size : Natural 0])
                    ([b-idx (in-list (car block-chain))]
                     [idx (in-naturals 0)])
            (define-values (pos size) ((cdr block-chain) b-idx))
            (define size++ (+ acc-size size))
            (vector-set! block-positions idx pos)
            (vector-set! block+sizes idx size++)
            size++)))
    
    (define total-size : Natural
      (if (> block-count 0)
          (min (or maybe-total max-size)
               max-size)
          0))
    
    (define (do-read [str : Bytes]) : Port-Reader-Plain-Datum
      (define available : Integer (- total-size consumed))
      
      (if (> available 0)
          (let* ([request (min available (bytes-length str))]
                 [bavail (- (vector-ref block+sizes block-idx) consumed)])
            (if (> bavail 0)
                (let ([amt (min request bavail)])
                  (when (or thread-safe? (= consumed 0))
                    (file-position /dev/srcin
                                   (+ (vector-ref block-positions block-idx)
                                      (if (> block-idx 0)
                                          (- consumed (vector-ref block+sizes (sub1 block-idx)))
                                          consumed))))
                  (define n (read-bytes-avail!* str /dev/srcin 0 amt))
                  (cond [(eq? n 0) (wrap-evt /dev/srcin (λ (x) 0))]
                        [(number? n) (set! consumed (+ consumed n)) n]
                        [(procedure? n) (do-read str)]
                        [else n]))
                (let ([nidx (unsafe-idx+ block-idx 1)])
                  (set! block-idx nidx)
                  (file-position /dev/srcin (vector-ref block-positions block-idx))
                  (do-read str))))
           eof))
    
    (define (try-again) : Port-Reader-Plain-Datum
      (wrap-evt
       (semaphore-peek-evt lock-semaphore)
       (λ [x] 0)))

    (make-input-port (or name (object-name /dev/srcin))
                     
                     (λ [[str : Bytes]] : Port-Reader-Plain-Datum
                       (call-with-semaphore lock-semaphore do-read try-again str))
                     #false
                     
                     (λ [] (when close-orig? (close-input-port /dev/srcin)))
                     
                     #false
                     #false
                     
                     (lambda () (port-next-location /dev/srcin))
                     (lambda () (port-count-lines! /dev/srcin))

                     /dev/srcin #| initial position |#)))

(define open-input-hexdump : (->* ()
                                  (#:width Byte #:cursor? Boolean #:name Any
                                   Input-Port Boolean (Option Byte))
                                  Input-Port)
  (lambda [#:width [width 16] #:cursor? [cursor? #true] #:name [name #false]
           [/dev/hexin (current-input-port)] [close-origin? #false] [error-byte #false]]
    (define lock-semaphore : Semaphore (make-semaphore 1))
    (define bs-line : Bytes (make-bytes width))
    (define bs-pos : Natural width)

    (define (load-line!) : Void
      (when (and cursor?) (read /dev/hexin))
      (define line (read-line /dev/hexin 'any))
      (when (string? line)
        (hexstring->bytes! line bs-line #:error-byte error-byte)
        (set! bs-pos 0)))
    
    (define (do-read [str : Bytes]) : Port-Reader-Plain-Datum
      (unless (< bs-pos width)
        (load-line!))

      (let ([available (- width bs-pos)])
        (if (> available 0)
            (let* ([request (bytes-length str)]
                   [amt (min request available)]
                   [bs-end (+ bs-pos amt)])
              (bytes-copy! str 0 bs-line bs-pos bs-end)
              (set! bs-pos bs-end)
              amt)
            eof)))
    
    (define (try-again) : Port-Reader-Plain-Datum
      (wrap-evt
       (semaphore-peek-evt lock-semaphore)
       (λ [x] 0)))

    (make-input-port (or name (object-name /dev/hexin))
                     
                     (λ [[str : Bytes]] : Port-Reader-Plain-Datum
                       (call-with-semaphore lock-semaphore do-read try-again str))
                     #false
                     
                     (λ [] (when close-origin? (close-input-port /dev/stdin)))
                     
                     #false
                     #false
                     
                     (lambda () (port-next-location /dev/stdin))
                     (lambda () (port-count-lines! /dev/stdin))

                     /dev/stdin #| initial position |#)))

(define open-input-memory : (->* (Bytes) (Natural Natural #:name Any) Input-Port)
  (lambda [memory [memory-start 0] [memory-end (bytes-length memory)] #:name [name #false]]
    (define cursor : Natural memory-start)
    
    (define (do-read [str : Bytes]) : Port-Reader-Datum
      (define available : Integer (- memory-end cursor))
      
      (cond [(<= available 0) eof]
            [else (let* ([count (min available (bytes-length str))]
                         [next-cursor (+ cursor count)])
                    (bytes-copy! str 0 memory cursor next-cursor)
                    (set! cursor next-cursor)
                    count)]))
    
    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (define count : Integer (min (- memory-end cursor skip) (bytes-length str)))
      
      (cond [(<= count 0) (if (and progress-evt (sync/timeout 0 progress-evt)) #false eof)]
            [else (let ([cp-pos (+ cursor skip)])
                    (bytes-copy! str 0 memory cp-pos (+ cp-pos count))
                    count)]))

    (make-input-port (or name '/dev/mmyin)
                     do-read do-peek
                     void #false #false #false
                     void (λ [] (+ cursor 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-output-hexdump : (->* ()
                                   (#:width Byte #:cursor-initial Natural #:cursor-width Byte
                                    #:cursor? Boolean #:binary? Boolean #:ascii? Boolean #:decimal-cursor? Boolean 
                                    #:upcase? Boolean #:name Any
                                    Output-Port Boolean)
                                   Output-Port)
  (lambda [#:width [width 32] #:cursor-initial [initial 0] #:cursor-width [cwidth 8]
           #:cursor? [cursor? #true] #:binary? [binary? #false] #:ascii? [ascii? #true] #:decimal-cursor? [dec-cursor? #false]
           #:upcase? [upcase? #false] #:name [name #false]
           [/dev/hexout (current-output-port)] [close-origin? #false]]
    (define snapshot (hexstate (make-bytes width 0) 0 initial))

    (define-values (q r) (quotient/remainder width 2))
    
    (define (hexdump [magazine : Bytes] [cursor : Natural] [n : Natural]) : Natural
      (define diff : Integer (- width n))
      (define cursor++ : Natural (+ cursor n))

      (unless (not cursor?)
        (write-string (~r cursor #:min-width cwidth #:base 16 #:pad-string "0") /dev/hexout)
        (write-char #\space /dev/hexout)
        (write-char #\space /dev/hexout))
      
      (for ([b (in-bytes magazine 0 n)]
            [i (in-naturals 1)])
        (if (not binary?)
            (write-string (byte->hexstring b upcase?) /dev/hexout)
            (write-string (byte->binstring b) /dev/hexout))
        (write-char #\space /dev/hexout)
        (when (and (= r 0) (= q i))
          (write-char #\space /dev/hexout)))
      
      (when (> diff 0)
        (write-string (~space (+ (* diff (if (not binary?) 3 9))
                                 (if (and (= r 0) (< n q)) 1 0)))
                      /dev/hexout))

      (unless (not ascii?)
        (write-char #\space /dev/hexout)
        (write-char #\| /dev/hexout)
        (for ([b (in-bytes magazine 0 n)]) (write-char (~char b #\.) /dev/hexout))
        (when (> diff 0) (write-string (~space diff) /dev/hexout))
        (write-char #\| /dev/hexout))

      (unless (not dec-cursor?)
        (write-char #\space /dev/hexout)
        (write-char #\space /dev/hexout)
        (write-string (~r cursor #:min-width cwidth #:base 10 #:pad-string "0") /dev/hexout)
        (write-char #\- /dev/hexout)
        (write-string (~r (sub1 cursor++) #:min-width cwidth #:base 10 #:pad-string "0") /dev/hexout))
      
      (newline /dev/hexout)
      cursor++)

    (define hexdump-write : (Output-Gate-Write hexstate)
      (lambda [bs start span s]
        (define width : Index (bytes-length (hexstate-magazine s)))
        (define payload : Natural (hexstate-payload s))
        (define magazine : Bytes (hexstate-magazine s))
        (define cursor : Natural (hexstate-cursor s))
        (define available : Natural (max (- width payload) 0))
        
        (if (< span available)
            (begin
              (bytes-copy! magazine payload bs start (+ start span))
              (values (hexstate magazine (+ payload span) cursor) span))
            (let ([start++ (+ start available)])
              (bytes-copy! magazine payload bs start start++)
              (values (hexstate magazine (+ payload available) cursor)
                      available)))))

    (define hexdump-flush : (Output-Gate-Flush hexstate)
      (lambda [s]
        (define width : Index (bytes-length (hexstate-magazine s)))
        (define payload : Natural (hexstate-payload s))
        (define magazine : Bytes (hexstate-magazine s))
        (define cursor : Natural (hexstate-cursor s))
        (define available : Natural (max (- width payload) 0))
        
        (if (> (hexstate-payload s) 0)
            (hexstate (hexstate-magazine s)
                      0
                      (hexdump magazine cursor payload))
            s)))

    (define (hexdump-position0 [s : hexstate]) : Positive-Integer
      (+ (hexstate-cursor s) 1))

    (define (hexdump-close [s : hexstate]) : Void
      (unless (not close-origin?)
        (close-output-port /dev/hexout)))

    (define gate : (Output-Gate hexstate)
      (make-output-gate #:name (or name (object-name /dev/hexout))
                        snapshot
                        ((inst make-output-gate<%> hexstate) #:write hexdump-write
                                                             #:flush hexdump-flush
                                                             #:close hexdump-close
                                                             #:position0 hexdump-position0)))

    (output-gate-port gate)))

(define open-output-memory : (->* (Bytes) (Natural Natural #:name Any) Output-Port)
  (lambda [memory [memory-start 0] [memory-end (bytes-length memory)] #:name [name #false]]
    (define cursor : Natural memory-start)
    
    (define (memory-write [bs : Bytes] [start : Natural] [end : Natural] [non-block/buffered? : Boolean] [enable-break? : Boolean]) : Integer
      (define src-size : Integer (- end start))

      (cond [(<= src-size 0) (set! cursor memory-start) 0]
            [(<= memory-end cursor) (error 'memory-write "out of memory")]
            [else (let ([available (- memory-end cursor)])
                    (if (>= available src-size)
                        (begin (bytes-copy! memory cursor bs start end)
                               (set! cursor (+ cursor src-size))
                               src-size)
                        (begin (bytes-copy! memory cursor bs start (+ start available))
                               (set! cursor memory-end)
                               available)))]))

    (make-output-port (or name '/dev/mmyout)
                      always-evt memory-write void
                      #false port-always-write-evt #false
                      #false void (λ [] (+ cursor 1)) #false)))

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

(define port-copy : (-> Input-Port Output-Port (Option Output-Port) * Void)
  (let ([buffer (make-bytes 4096)])
    (lambda [/dev/stdin /dev/stdout . /dev/extouts]
      (define stdouts : (Listof Output-Port) (cons /dev/stdout (filter output-port? /dev/extouts)))

      (with-handlers ([exn:break? void])
        (let sync-read-copy-loop ()
          (define n (read-bytes-avail! buffer /dev/stdin))
          (cond
            [(exact-integer? n)
             (for ([stdout (in-list stdouts)])
               (let write-loop ([bytes-written 0])
                 (unless (= bytes-written n)
                   (define c2 (write-bytes-avail buffer stdout bytes-written n))
                   (flush-output stdout)
                   (write-loop (+ bytes-written c2)))))
             (sync-read-copy-loop)]
            [(procedure? n)
             (define-values (l col p) (port-next-location /dev/stdin))
             (define v (n l col p 0))
             (for ([stdout (in-list stdouts)])
               (when (port-writes-special? stdout)
                 (write-special v stdout)))
             (sync-read-copy-loop)]
            [else (void 'eof)]))))))
  
(define port-copy/usrin : (->* (Input-Port Output-Port)
                               (#:timeout Positive-Real #:done? (-> Boolean))
                               #:rest (Option Output-Port)
                               Void)
  (let ([buffer (make-bytes 4096)])
    (lambda [/dev/stdin /dev/stdout #:timeout [timeout 0.1] #:done? [done? λfalse] . /dev/extouts]
      (define all-outs : (Listof Output-Port) (cons /dev/stdout (filter output-port? /dev/extouts)))
      (define size : Index (bytes-length buffer))

      (when (and (eq? digimon-system 'windows) (terminal-port? /dev/stdin))
        ; Windows Console makes it impossible to report stdin event properly for Racket Virtual Machine,
        ; Although this trick does not work for tasks running longer than timeout.
        (sync/timeout/enable-break timeout never-evt))

      (let sync-read-copy-loop ([outs : (Listof Output-Port) all-outs])
        (when (pair? outs)
          (unless (done?)
            (define which (sync/timeout/enable-break timeout /dev/stdin))
            (if (eq? which /dev/stdin)
                (let ([n (read-bytes-avail!* buffer /dev/stdin 0 size)])
                  (cond [(eof-object? n) '#:return (sync-read-copy-loop null)]
                        [(and (index? n) (> n 0)) (sync-read-copy-loop (ports-filter-write outs buffer 0 n))]
                        [else '#:ignore (sync-read-copy-loop outs)]))
                (sync-read-copy-loop outs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-random-access? : (-> (U Input-Port Output-Port) Boolean)
  (lambda [/dev/stdio]
    (or (string-port? /dev/stdio)
        (with-handlers ([exn:fail? (λ [[e : exn]] #false)])
          (file-position /dev/stdio (file-position /dev/stdio))
          #true))))
