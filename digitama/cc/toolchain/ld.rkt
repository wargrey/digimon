#lang typed/racket/base

(require "../linker.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (eq? (system-type 'os) 'windows)
  (c-register-linker 'ld
                       (make-ld 'ld '(flags libpath libraries infiles outfile))))
