#lang typed/racket/base

(require digimon/packbits)
(require digimon/spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define in : Bytes (bytes #xFE #xAA #x02 #x80 #x00 #x2A #xFD #xAA #x03 #x80 #x00 #x2A #x22 #xF7 #xAA))
(define out : Bytes (bytes #xAA #xAA #xAA #x80 #x00 #x2A #xAA #xAA #xAA #xAA #x80 #x00
                           #x2A #x22 #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA))

(define-feature RLE #:do
  (context "Tests from wikipedia" #:do
    (it "should be okay for functional version" #:do
      (expect-equal out (unpackbits in)))
    (it "should be okay for in-place version" #:do
      (expect-equal out (unpackbits (bytes-length out) in)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define out! : Bytes (make-bytes (bytes-length out)))
  (define end (bytes-length in))
  (collect-garbage)
  (time (for ([i (in-range 100000)]) (unpackbits! out! 0 in 0 end)))
  (collect-garbage)
  (time (for ([i (in-range 100000)]) (unpackbits* in 0 end)))

  (void (spec-prove RLE)))
