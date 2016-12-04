#lang typed/racket

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/log : Logger (make-logger 'digital-world #false))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define /dev/zero : Input-Port
  (make-input-port '/dev/zero
                   (λ [[bs : Bytes]]
                     (bytes-fill! bs #x00)
                     (bytes-length bs))
                   #false
                   void))

#|
(define /dev/urandom : Input-Port
  (make-input-port '/dev/urandom
                   (λ [[bs : Bytes]]
                     (let ([bsize (bytes-length bs)])
                       (bytes-copy! bs 0 (crypto-random-bytes bsize))
                       bsize))
                   #false
                   void))
|#

(void (print-boolean-long-form #true)
      (current-logger /dev/log)
        
      ;;; Ignore DrRacket's convention. But don't change "compiled" since
      ;;; the compiler checks the bytecodes in the core collection
      ;;; which have already been compiled into <path:compiled/>.
      (use-compiled-file-paths (list (build-path "compiled"))))
