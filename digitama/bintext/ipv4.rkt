#lang typed/racket/base

;;; https://tools.ietf.org/html/rfc1071

(provide (all-defined-out))

(require "../unsafe/ops.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define update-sum : (-> Index Bytes Index Index Index)
  (lambda [sum message start end0]
    (define count : Index (unsafe-idx- end0 start))
    
    (define-values (#{H_ : Index} #{end : Fixnum})
      (cond [(even? count) (values 0 end0)]
            [else (let ([--end (unsafe-fx- end0 1)])
                    (values (unsafe-bytes-ref message --end) --end))]))

    (let ipv4 ([idx : Index start]
               [H : Index (unsafe-idx+ (unsafe-idxrshift sum 8) H_)]
               [L : Index (unsafe-idxand sum #xFF)])
      (if (unsafe-fx< idx end)
          (ipv4 (unsafe-idx+ idx 2)
                (unsafe-idx+ H (unsafe-bytes-ref message idx))
                (unsafe-idx+ L (unsafe-bytes-ref message (unsafe-idx+ idx 1))))
          (unsafe-uint16-not (let carry ([HL : Index (unsafe-idx+ (unsafe-fxlshift H 8) L)])
                               (if (unsafe-fx> HL #xFFFF)
                                   (carry (unsafe-idx+ (unsafe-idxand HL #xFFFF)
                                                       (unsafe-idxrshift HL 16)))
                                   HL)))))))
