#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/port))

(require racket/port)

(require typed/racket/random)

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
  (lambda [/dev/srcin [iobox ((inst box Natural) 0)] [name '/dev/tpkib]]
    (make-input-port name
                     (λ [[s : Bytes]] : (U EOF Exact-Positive-Integer)
                       (define peeked : Natural (unbox iobox))
                       (define r (peek-bytes! s peeked /dev/srcin))
                       (set-box! iobox (+ peeked (if (number? r) r 1))) r)
                     #false
                     void)))

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
