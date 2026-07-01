#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unpackbits : (case-> [Bytes -> Bytes]
                             [Bytes Integer Integer -> Bytes]
                             [Integer Bytes -> Bytes]
                             [Integer Bytes Integer Integer -> Bytes])
  (case-lambda
    [(src) (unpackbits* src 0 (bytes-length src))]
    [(src start end) (unpackbits* src start end)]
    [(size src) (let ([dest (make-bytes size)]) (unpackbits! dest 0 src 0 (bytes-length src)))]
    [(size src start end) (let ([dest (make-bytes size)]) (unpackbits! dest 0 src start end))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unpackbits* : (-> Bytes Integer Integer Bytes)
  (lambda [src start end]
    (cond [(fx>= start end) #""]
          [else (let ([header (bytes-ref src start)])
                  (cond [(fx> header 128)
                         (let ([one (fx+ start 1)])
                           (bytes-append (make-bytes (fx+ (fx- 256 header) 1) (bytes-ref src one))
                                         (unpackbits* src (fx+ one 1) end)))]
                        [(fx< header 128)
                         (let* ([one (fx+ start 1)]
                                [next-start (fx+ header (fx+ one 1))])
                           (bytes-append (subbytes src one next-start)
                                         (unpackbits* src next-start end)))]
                        [else ; useless, but for compatibility
                         (unpackbits* src (fx+ start 1) end)]))])))

(define unpackbits! : (-> Bytes Integer Bytes Integer Integer Bytes)
  (lambda [dest offset src start end]
    (cond [(fx>= start end) dest]
          [else (let ([header (bytes-ref src start)])
                  (cond [(fx> header 128)
                         (let* ([one (fx+ start 1)]
                                [datum (bytes-ref src one)]
                                [count (fx+ (fx- 256 header) 1)]
                                [dest-next (fx+ offset count)])
                           (let fill! ([idx offset])
                             (when (fx< idx dest-next)
                               (bytes-set! dest idx datum)
                               (fill! (fx+ idx 1))))
                           (unpackbits! dest dest-next src (fx+ one 1) end))]
                        [(fx< header 128)
                         (let* ([one (fx+ start 1)]
                                [count (fx+ header 1)]
                                [src-next (fx+ one count)])
                           (bytes-copy! dest offset src one src-next)
                           (unpackbits! dest (fx+ offset count) src src-next end))]
                        [else ; useless, but for compatibility
                         (unpackbits! dest offset src (fx+ start 1) end)]))])))
