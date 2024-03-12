#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))
