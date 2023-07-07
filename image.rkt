#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/stdio.rkt")

(require "digitama/toolchain/out/image.rkt")

(require "digitama/toolchain/out/mach.rkt")
(require "digitama/toolchain/out/pecoff.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-c-image : (-> (U Input-Port Path-String) C:Image)
  (lambda [/dev/stdin]
    (if (input-port? /dev/stdin)
        (cond [(mach-image? /dev/stdin) (read-mach-image /dev/stdin)]
              [(pecoff-image? /dev/stdin) (read-pecoff-image /dev/stdin)]
              [else (raise (make-exn:fail:unsupported
                            (format "unknown image format: ~a" (object-name /dev/stdin))
                            (current-continuation-marks)))])
        (call-with-input-file* /dev/stdin read-c-image))))
